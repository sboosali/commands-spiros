{-# LANGUAGE DeriveFunctor, QuasiQuotes, RecordWildCards #-}
-- | (you should read the source for documentation: just think of this module as a config file)
module Commands.Plugins.Spiros.Shim.QQ where
import Commands.Plugins.Spiros.Extra (CanInterpolate) 

import           Commands.Frontends.Dragon13

import           Text.InterpolatedString.Perl6


getShim :: (CanInterpolate t) => ShimR t -> t
getShim ShimR{..} = [qc|
#-*- coding: utf-8 -*-
# _commands.py

# natlink13 library
import natlink  # a DLL
import natlinkmain
import natlinkutils

# standard library
import time
import json
import urllib2
import traceback




# enums

'''
Python 2.7 
inspired by http://stackoverflow.com/questions/36932/how-can-i-represent-an-enum-in-python
''' 
def enum(name, raw_values):
    cases = raw_values.split()

    T = type(name, (int,), dict())  # int provides fast equality 

    T.__cases__ = cases   # provides printable constructors 
    T.__str__ = lambda self: name + "." + T.__cases__[self]

    for (index,case) in enumerate(cases): # provides constructors 
        setattr(T, case, T(index))

    return T

''' e.g.
Numbers = enum("Numbers", "ONE TWO THREE")
print Numbers.TWO
# Numbers.TWO
print str(Numbers.TWO)
# "Numbers.TWO" 
print int(Numbers.TWO) 
# 1 
print type(Numbers.TWO) 
# <type 'Numbers'> 
print type(Numbers) 
# <type 'type'>
print Numbers.__cases__
# ['ONE', 'TWO', 'THREE']
print Numbers.TWO == Numbers.TWO
# True 
print Numbers.TWO != 2
# True 
print Numbers.TWO is Numbers.TWO
# True 
'''








# types 

MagicState = enum("MagicState", "")

CorrectionStatus = enum("CorrectionStatus", "Success Heterophonic InvalidWord")

MicrophoneState = enum("MicrophoneState", "On Sleeping Off")






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
correctable_rule = '''<correctable> exported = "fix-it" | correct;'''

# see handle_correcting(...)
correcting_export = "correcting"
correcting_rule = '''
<correcting> exported
 = yes 
 | <dgndictation>
 | zero | one | two | three | four | five | six | seven | eight | nine;
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

class NarcissisticGrammar(natlinkutils.GrammarBase):
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
    previous_results = None        # set by recognition, initialized during first call to gotResultsObject()
    correcting_results = None        # set by "correcting", must be None otherwise 
    current_mode = "normal"

    def initialize(self):
        self.set_rules  (all_rules)
        self.set_exports(active_exports, exclusive=1)
        self.set_lists  (all_lists)
        self.doOnlyGotResultsObject = True   # when True, aborts all processing after calling gotResultsObject

    # called when speech is detected before recognition begins.
    # moduleInfo is just the current window in Windows
    # def gotBegin(self, moduleInfo):

    # called several times as speech is being detected 
    # def gotHypothesis(self, words):

    # (this callback is where most of the work is done)
    # 
    # recognitionType = self | reject | other 
    def gotResultsObject(self, recognitionType, resultsObject):

        print "---------- gotResultsObject ----------"
        print "recognitionType =", recognitionType
        if not recognitionType: return

        raw   = next(get_results(resultsObject), [])
        words = munge_recognition(raw)

        # print 'resultsObject =',resultsObject
        print 'raw =', raw

        try:

#            if raw and raw!=["Microphone","off"]:
#                self.activateSet([],exclusive=0)
#                natlink.recognitionMimic(["Microphone","off"])
#            else:
#                self.activateSet([microphone_export, H_EXPORT], exclusive=1)

            (should_send_request, should_change_mode) = try_magic_handlers(self,words)

            if should_change_mode: 
                self.current_mode = should_change_mode 

            if self.current_mode == "correcting" or self.current_mode == "dnsmode/dictating" or self.current_mode == "microphone/sleeping": 
                self.should_request = False 
            if self.current_mode == "normal":
                self.should_request = True  

            # we set it only the first time we enter "correcting" mode 
            # i.e. we don't want to try to "correct the correction"
            if self.current_mode == "correcting" and self.correcting_results == None: 
                self.correcting_results = self.previous_results
                # TODO must enable requests to pass through, after the first "fix-it" is ignored 
                results_identity = id(self.correcting_results)
                raw_hypotheses = list(get_results(self.correcting_results))
                print_hypotheses(raw_hypotheses)
                hypotheses = [munge_recognition(hypothesis) for hypothesis in raw_hypotheses] 
                post_hypotheses(results_identity, hypotheses) 

#             if self.current_mode == "normal": # TODO 
#                 self.previous_results = resultsObject
            self.previous_results = resultsObject

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
                post_recognition(words, self.current_mode) 
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
            print "previous_results   =", self.previous_results
            print "correcting_results =", self.correcting_results

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

# recognition :: [String] 
def post_recognition(recognition, mode): 
    url      = "%s/recognition/" % (server_address,)        # TODO parameterize API
    data     = json.dumps(recognition) 
    request  = urllib2.Request(url, data, \{"Content-Type": "application/json"})
    print 'url   =', url
    print 'data  =', data 
    response = urllib2.urlopen(request)
    return response 

# hypotheses :: [[String]] 
def post_hypotheses(identifier, hypotheses):
    url      = "%s/hypotheses/" % (server_address,)        # TODO parameterize API
    # data     = json.dumps([identifier, hypotheses])
    data     = json.dumps(hypotheses)
    request  = urllib2.Request(url, data, \{"Content-Type": "application/json"})
    print 'url   =', url
    print 'data  =', data 
    response = urllib2.urlopen(request)
    return response 

# 
def handleDNSResponse(grammar, response):
    pass 





# magic 

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

# def read_microphone(datum):   
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

# 
def set_microphone(state):
    if   state == MicrophoneState.On: 
        natlink.setMicState("on") 

    elif state == MicrophoneState.Sleeping: 
        natlink.setMicState("sleeping")

    elif state == MicrophoneState.Off: 
        natlink.setMicState("off")

    else: 
        raise TypeError("set_microphone", state) 

# 
def handle_dnsmode(grammar,datum):

    # print 'handle_dnsmode(', datum, ')'

    if   datum == "dictating":
        grammar.activateSet([dnsmode_export],exclusive=0)
        natlinkmain.recognitionMimic(["Dictation","mode"]) 
        return "dnsmode/dictating"          # TODO lol

    elif datum == "commanding":
        grammar.activateSet(active_exports,exclusive=1)
        return "normal"           # TODO lol

    else:
        return None

# 
def handle_correctable(grammar,datum):

    if   datum == "fix-it" or datum == "correct":
        grammar.activateSet([correcting_export],exclusive=1)
        return "correcting"     # change state 

    else:
        return None

# 
def handle_correcting(grammar,datum,corrected_recognition=None):

    if   datum == "yes":

        grammar.activateSet(active_exports,exclusive=1)
        recognition = first_result(grammar.correcting_results)
        print     "original_recognition   =", recognition 

        if recognition is not None:
            print "corrected_recognition  =", corrected_recognition 
            try: 
                correction_status = perform_correction(grammar.correcting_results, corrected_recognition)
                print "correction_status      =", correction_status
            finally: 
                grammar.correcting_results = None   # even if it fails 
                return "normal"                     # even if it fails 

        return "normal"      # change state 

    else:
        return None 

# 
def perform_correction(theRecognition, theCorrection): 
    try: 
        status = theRecognition.correction(theCorrection) 
        return CorrectionStatus.Success if status else CorrectionStatus.Heterophonic
    except natlink.InvalidWord: 
        return CorrectionStatus.InvalidWord 

# 
def handle_readable(grammar,datum):

    if   datum == "reading":
        grammar.activateSet([reading_export],exclusive=1)
        return "reading"     # change state 

    else:
        return None

# 
def handle_reading(grammar,datum):

    if   datum == "speaking":
        grammar.activateSet(active_exports,exclusive=1)
        return "normal"     # change state 

    else:
        return None

# 
def isUnicode(data):
    try:
        for word in data:
            word.decode('utf8')
        return True
    except UnicodeDecodeError as e:
        print e
        print traceback.format_exc()
        return False

# 
def isNoise(data):
    return data in [["the"],["if"],["him"],["A"],["that"],["a"], ["she"]] #TODO hack, noise tends to be recognized as these short single words












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
    return [encode_windows(word).split('\\\\')[0] for word in words]

# http://stackoverflow.com/questions/12468179/unicodedecodeerror-utf8-codec-cant-decode-byte-0x9c
def encode_windows(s): 
    return s.decode('cp1252').encode('utf-8')

# http://stackoverflow.com/questions/1685221/accurately-measure-time-python-function-takes
def timeit(message, callback, *args, **kwargs):
    before = time.clock()
    result = callback(*args,**kwargs)
    after = time.clock()
    print message, ': ', (after - before) * 1000, 'ms'
    return result

def print_hypotheses(hypotheses):
    for (index, hypothesis) in enumerate(hypotheses):
        print "hypothesis %d = %s" % (index, munge_recognition(hypothesis))

# the microphone state changes independently of the "command mode".  
def is_mode_awake(grammar):
    return not (grammar.current_mode in ["microphone/sleeping", "microphone/off"])





# boilerplate

GRAMMAR = None # mutable global

def load():
    global GRAMMAR
    # automatically reload on file change (not only when microphone toggles on)
    natlinkmain.setCheckForGrammarChanges(1)
    GRAMMAR = NarcissisticGrammar()
    GRAMMAR.initialize()

def unload():
    global GRAMMAR
    if GRAMMAR:
        GRAMMAR.unload()
    GRAMMAR = None

load()
#|] -- trailing comment is a hack, which comments out the Unicode garbage that trails the clipboard contents
