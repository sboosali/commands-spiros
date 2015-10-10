{-# LANGUAGE DeriveFunctor, QuasiQuotes, RankNTypes, RecordWildCards #-}
-- | (you should read the source for documentation: just think of this module as a config file)
module Commands.Plugins.Spiros.Shim where

import           Commands.Frontends.Dragon13

import           Text.InterpolatedString.Perl6

import           GHC.Exts                        (IsString)


getShim :: (IsString t, Monoid t) => ShimR t -> t
getShim ShimR{..} = [qc|
#-*- coding: utf-8 -*-
# _commands.py

# natlink13 library
from natlinkmain import (setCheckForGrammarChanges)
from natlinkutils import (GrammarBase)
import natlink  # a DLL
import natlinkmain
# import natlinkutils

# standard library
import time
import json
import urllib2
import traceback



# helpers

def merge_dicts(x, y):
    '''Given two dicts, merge them into a new dict as a shallow copy.'''
    z = dict()
    z.update(x)
    z.update(y)
    return z



# interpolated from "H"askell

H_EXPORT = {__export__}
H_RULES  = {__rules__}
H_LISTS  = {__lists__}
H_SERVER_HOST = {__serverHost__}  
H_SERVER_PORT = {__serverPort__}

# e.g. for debugging
# H_EXPORT = 'test'
# H_RULES  = '''<test> exported = \{test};'''
# H_LISTS  = \{'test', ['upcase region']}
# H_SERVER_HOST = "192.168.56.1"
# H_SERVER_PORT = '8666'

server_address = "http://%s:%s/" % (H_SERVER_HOST, H_SERVER_PORT)

# see handle_microphone(...)
# "wake up" is somewhat redundant with "mike on", but necessary because when the Dragon microphone puts itself to sleep, only "wake up" will wake it back up, and we handle that recognition to enable any disabled grammars in handle_microphone().
microphone_export = "microphone"
microphone_rule = '''<microphone> exported = wake up | mike on | mike off | go to sleep | mike dead ;'''

# see handle_dnsmode(...)
dnsmode_export = "dnsmode"
dnsmode_rule = '''<dnsmode> exported = dictating | commanding;'''

# see handle_correctable(...)
correctable_export = "correctable"
correctable_rule = '''<correctable> exported = fix;'''

# see handle_correcting(...)
correcting_export = "correcting"
correcting_rule = '''
<correcting> exported
 = <dgndictation>
 | nil | zero | one | two | three | four | five | six | sev | seven | eight | nine;
'''

readable_export = "readable"
readable_rule = '''<readable> exported = reading ;'''

reading_export = "reading"
reading_rule = '''<reading> exported = \{reading} | speaking ;'''
reading_lists = dict(reading=["scroll", "scroll down", "scroll up"])

active_exports =           [microphone_export, dnsmode_export, correctable_export, readable_export, H_EXPORT]
active_rules   = '\n'.join([microphone_rule,   dnsmode_rule,   correctable_rule,   readable_rule, H_RULES]) 

all_exports =           [microphone_export, dnsmode_export, correctable_export, correcting_export, reading_export, readable_export, H_EXPORT]
all_rules   = '\n'.join([microphone_rule,   dnsmode_rule,   correctable_rule,   correcting_rule,   reading_rule,   readable_rule, H_RULES]) 
all_lists   = merge_dicts(H_LISTS,reading_lists)

# TODO re-factor the microphone/dnsmode/correct/correcting grammars into their own grammar objects (GrammarBase)






# the grammar

class NarcissisticGrammar(GrammarBase):
    ''' 'Narcissistic' because:

    * load(.., allResults=1)     means: every recognition triggers gotResultsObject
    * load(.., hypothesis=1)     means: every hypothesis, before the recognition, triggers gotHypothesis
    * activate(.., exclusive=1)  means: deactivate every other non-exclusive rule

    (when both flags are set on load, NarcissisticGrammar.gotResultsObject is called on
    every recognition of every exclusive rule, including this class's rules
    of course, though I only expect this class to be active).

    '''

    gramSpec = None        # initialized in set_rules(...) 
    should_request = True
    previous_results_object = None        # set by recognition, initialized during first call to gotResultsObject()
    current_mode = "normal"

    def initialize(self):
        self.set_rules  (all_rules)
        self.set_exports(active_exports, exclusive=1)
        self.set_lists  (all_lists)
        self.doOnlyGotResultsObject = True   # when True, aborts all processing after calling gotResultsObject

    # called when speech is detected before recognition begins.
    # moduleInfo is just the current window in Windows
    def gotBegin(self, moduleInfo):
        # handleDGNContextResponse(timeit("/context", urlopen, ("%s/context" % server_address), timeout=0.1))
        # TODO parameterize "context" API

        print
        print
        print
        print "---------- gotBegin -------------------"


    def gotHypothesis(self, words):
        print
        print "---------- gotHypothesis -------------"
        print words


    # (this callback is where most of the work is done)
    # 
    # recognitionType = self | reject | other 
    def gotResultsObject(self, recognitionType, resultsObject):

        print "---------- gotResultsObject ----------"
        print "recognitionType =", recognitionType
        if not recognitionType: return

        words = next(get_results(resultsObject), [])
        data  = munge_recognition(words)
        url   = "%s/recognition/" % (server_address,)        # TODO parameterize "recognition" API

        # print 'resultsObject =',resultsObject
        print 'words =', words
        print 'url   =', url

        try:

#            if words and words!=["Microphone","off"]:
#                self.activateSet([],exclusive=0)
#                natlink.recognitionMimic(["Microphone","off"])
#            else:
#                self.activateSet([microphone_export, H_EXPORT], exclusive=1)

            (should_send_request, should_change_mode) = try_magic_handlers(self,data)
            self.should_request = False if (should_change_mode == "dnsmode/dictating" or should_change_mode == "correcting" or should_change_mode == "microphone/sleeping") else (True if should_change_mode == "normal" else self.should_request)
            self.current_mode   = should_change_mode or self.current_mode

            self.previous_results_object = resultsObject if self.current_mode == "normal" else self.previous_results_object

            # the microphone falls asleep during silence,
            # but doesn't wake up when a command is uttered. 
            # we turn the microphone back on, as long as it should be on. 
            if is_mode_awake(self):
                natlink.setMicState("on")

            # the microphone may have been toggled manually by the GUI,
            # or fallen asleep automatically.  
            if self.current_mode.startswith("microphone"):
                if natlink.getMicState() == "on":
                    should_change_microphone_mode = handle_microphone(self,"mike on")
                    if should_change_microphone_mode:
                        self.current_mode = should_change_microphone_mode
                if natlink.getMicState() == "sleeping":
                    should_change_microphone_mode = handle_microphone(self,"mike off")
                    if should_change_microphone_mode:
                        self.current_mode = should_change_microphone_mode

            print "current_mode   =", self.current_mode
            print "should_request =", self.should_request
            if should_send_request and self.should_request:
                print 'data  =', json.dumps(data)
                request  = urllib2.Request(url, json.dumps(data), \{"Content-Type": "application/json"})
                response = urllib2.urlopen(request)
            pass

        except Exception as e:
            print
            print "---------- error ------------------"
            print "sending the request and/or handling the response threw:"
            print e
            print traceback.format_exc()

        # don't print until the request is sent the response is handled
        try:
            print
            print "status  =", response.getcode()
            print "body    =", list(response)
#             for (index, hypothesis) in enumerate(get_results(resultsObject)):
#                 print "hypothesis %d = %s" % (index, munge_recognition(hypothesis))

        except NameError:
            print
        except Exception as e:
            print
            print "---------- error ------------------"
            print e
            print traceback.format_exc()


    # for debugging only, shows whether specific rules (rather than the generic dgndictation) are matching the recognition
    # not called when (self.doOnlyGotResultsObject=True)
    def gotResults(self, words, fullResults):
        print
        print "---------- gotResults ----------"
        print "fullResults =", fullResults


    # TODO    must it reload the grammar?
    def set_rules(self, rules):
        self.gramSpec = rules
        self.load(rules, allResults=1, hypothesis=1)


    # activateSet is idempotent, unlike activate
    def set_exports(self, exports, exclusive):
        self.activateSet(exports, exclusive=exclusive)


    # TODO must it reload the grammar?
    def set_lists(self, lists):
        for (lhs, rhs) in lists.items():
            self.setList(lhs, rhs)





# API

# TODO             handleDGNUpdate(grammar, response)
def handleDGNUpdate(grammar, response):
    pass 


# data :: [String] 
# datum :: String
def try_magic_handlers(grammar,data):

    # truthy if the recognition is good enough
    is_recognition_good = data and isUnicode(data) and not isNoise(data)

    # a Maybe String
    should_change_mode = None

    if is_recognition_good:
        datum = " ".join(data)

        # truthy if the recognition has been handled "magically"
        # by the client, and shouldn't be handled by the server TODO make this API explicit in servant
        should_change_mode = handle_abrogation(data) or handle_correctable(grammar,datum) or handle_correcting(grammar,datum) or handle_microphone(grammar,datum) or handle_dnsmode(grammar,datum) or handle_readable(grammar,datum) or handle_reading(grammar,datum)
    should_send_request = is_recognition_good and not should_change_mode

    return (should_send_request, should_change_mode) 


# you can abort any recognition by saying "abrogate". as a rare word, it will rarely be dictated (except for bootstrapping). if you must say it, it supports the "say" prefix (i.e. recognize literally everything after it). 
def handle_abrogation(data):

    # TODO the first predicate is a hack to let you say the magic word
    # helps with development 
    if data[0] != "say" and "abrogate" in data:
        return "normal"

    else:
        return None 


# returns true if it matched the recognition (and executed the magic action).
# in which case, don't send a request to the server to execute any non-magic actions.
# "mike off" deactivates all grammars besides the microphone grammer, "putting the microphone to sleep".
def handle_microphone(grammar,datum):

    if  datum == "wake up" or datum == "mike on":
        natlink.setMicState("on") 
        grammar.activateSet([microphone_export, H_EXPORT], exclusive=1)
        return "normal"

    elif datum == "go to sleep" or datum == "mike off":
        natlink.setMicState("sleeping")
        grammar.activateSet([microphone_export],exclusive=1)
        return "microphone/sleeping"       # change state

    elif datum == "mike dead":
        natlink.setMicState("off")
        return "microphone/off"       # change state

    else:
        return None 


def handle_dnsmode(grammar,datum):

#    print 'handle_dnsmode(', datum, ')'

    if   datum == "dictating":
        grammar.activateSet([dnsmode_export],exclusive=0)
        natlinkmain.recognitionMimic(["Dictation","mode"]) 
        return "dnsmode/dictating"          # TODO lol

    elif datum == "commanding":
        grammar.activateSet(active_exports,exclusive=1)
        return "normal"           # TODO lol

    else:
        return None


def handle_correctable(grammar,datum):

    if   datum == "fix":
        grammar.activateSet([correcting_export],exclusive=1)
        return "correcting"     # change state 

    else:
        return None


def handle_correcting(grammar,datum):

    if   datum == "zero":
        grammar.activateSet(active_exports,exclusive=1)
        recognition = first_result(grammar.previous_results_object)
        print     "original_recognition   =", recognition 
        if recognition is not None:
            did_correction_succeed = grammar.previous_results_object.correction(recognition)
            print "corrected_recognition  =", recognition 
            print "did_correction_succeed =", did_correction_succeed
        return "normal"      # change state 

    else:
        return None 


def handle_readable(grammar,datum):

    if   datum == "reading":
        grammar.activateSet([reading_export],exclusive=1)
        return "reading"     # change state 

    else:
        return None


def handle_reading(grammar,datum):

    if   datum == "speaking":
        grammar.activateSet(active_exports,exclusive=1)
        return "normal"     # change state 

    else:
        return None


def isUnicode(data):
    try:
        for word in data:
            word.decode('utf8')
        return True
    except UnicodeDecodeError as e:
        print e
        print traceback.format_exc()
        return False


def isNoise(data):
    return data in [["the"],["if"],["him"],["A"],["that"],["a"]] #TODO hack, noise tends to be recognized as these short single words





# helpers


# current time in milliseconds
def now():
    return int(time.clock() * 1000)

def first_result(resultsObject):
    return next(get_results(resultsObject), None)

# returns an iterator of words (strings)
# 
# faster than get_results_verbose, returning only one of the WordInfo fields
def get_results(resultsObject):
    try:
        for choice in xrange(10):
            yield resultsObject.getWords(choice)
    except:
        return


# returns an iterator of the WordInfo's
# 
# None means no recognition
# the first is the recognition 
# the rest are the hypotheses, which may be used to correct the recognition 
# there are nine or ten hypotheses at most 
def get_results_verbose(resultsObject):
    try:
        for choice in xrange(10):
            yield resultsObject.getWordInfo(choice)
    except:
        return


def munge_recognition(words):
    '''
    >>> munge_recognition(['spell', 'a\\\\spelling-letter\\\\A', ',\\\\comma\\\\comma', 'a\\\\determiner', 'letter'])
    ["spell", "A", ",", "a", "letter"]
    '''
    return [word.split('\\\\')[0] for word in words]


# http://stackoverflow.com/questions/1685221/accurately-measure-time-python-function-takes
def timeit(message, callback, *args, **kwargs):
    before = time.clock()
    result = callback(*args,**kwargs)
    after = time.clock()
    print message, ': ', (after - before) * 1000, 'ms'
    return result


'''
from natlink.txt:

    wordName - same as returned by ResObj.getChoice
    cfgParse - same as returned by ResObj.getChoice
    wordScore - for the first word of the result, this is the score
        of this choice; lower is better; zero for other words
    startTime - start of word in milliseconds from start of utterance
    endTime - end of word in milliseconds from start of utterance
    engineInfo - integer (actually a DWORD) properties of the word
    wordPron - first pronunciation for the word; not necessarily
        the pronunciation which was recognized
        however, wordPron is always empty, I think to protect intellectual property. 

'''
class WordInfo(object): pass


# the microphone state changes independently of the "command mode".  
def is_mode_awake(grammar):
    return not (grammar.current_mode in ["microphone/sleeping", "microphone/off"])





# boilerplate

GRAMMAR = None # mutable global

def load():
    global GRAMMAR
    setCheckForGrammarChanges(1) # automatically reload on file change (not only when microphone toggles on)
    GRAMMAR = NarcissisticGrammar()
    GRAMMAR.initialize()

def unload():
    global GRAMMAR
    if GRAMMAR:
        GRAMMAR.unload()
    GRAMMAR = None

load()
#|] -- trailing comment is a hack, which comments out the Unicode garbage that trails the clipboard contents
