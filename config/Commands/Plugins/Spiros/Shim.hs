{-# LANGUAGE DeriveFunctor, QuasiQuotes, RankNTypes, RecordWildCards #-}
-- | (you should read the source for documentation: just think of this module as a config file)
module Commands.Plugins.Spiros.Shim where

import           Commands.Frontends.Dragon13 hiding (getShim)

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

# interpolated from "H"askell

H_RULES  = {__rules__}
H_LISTS  = {__lists__}
H_EXPORT = {__export__}
H_SERVER_HOST = {__serverHost__}  
H_SERVER_PORT = {__serverPort__}

# e.g. for debugging
# H_RULES  = '''<test> exported = \{test};'''
# H_LISTS  = \{'test', ['upcase region']}
# H_EXPORT = 'test'
# H_SERVER_HOST = "192.168.56.1"
# H_SERVER_PORT = '8666'

server_address = "http://%s:%s/" % (H_SERVER_HOST, H_SERVER_PORT)

# see handle_microphone(...)
# "wake up" is somewhat redundant with "mike on", but necessary because when the Dragon microphone puts itself to sleep, only "wake up" will wake it back up, and we handle that recognition to enable any disabled grammars in handle_microphone().
microphone_rule = '''<microphone> exported = wake up | mike on | mike off | mike dead ;'''
microphone_export = "microphone"

# see handle_mode(...)
mode_rule = '''<mode> exported = dictating | commanding;'''
mode_export = "mode"

all_rules   =  microphone_rule  + mode_rule  + H_RULES
all_exports = [microphone_export, mode_export, H_EXPORT]


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

    def initialize(self):
        self.set_rules(all_rules, all_exports, exclusive=1)
        self.set_lists(H_LISTS)
        self.doOnlyGotResultsObject = True # aborts all processing after calling gotResultsObject

    # called when speech is detected before recognition begins.
    def gotBegin(self, moduleInfo):
        # handleDGNContextResponse(timeit("/context", urlopen, ("%s/context" % server_address), timeout=0.1))
        # TODO parameterize "context" API

        print
        print
        print "-  -  -  -  gotBegin  -  -  -  -"
        # moduleInfo is just the current window in Windows

    def gotHypothesis(self, words):
        print
        print "---------- gotHypothesis -------------"
        print words

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

            if should_request(self,data):
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
            print "status =", response.getcode()
            print "body   =", response
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
        print "---------- gotResultsObject ----------"
        print "fullResults =", fullResults

    # TODO    must it reload the grammar?
    # TODO    should include export for safety?
    def set_rules(self, rules, exports, exclusive):
        self.gramSpec = rules
        self.load(rules, allResults=1, hypothesis=1)
        self.set_exports(exports, exclusive)

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
def should_request(grammar,data):
    isRecognitionGood = data and isUnicode(data) and not isNoise(data)
    if isRecognitionGood:
        datum = " ".join(data)
        isRecognitionMagical = handle_abrogation(data) or handle_microphone(grammar,datum) or handle_mode(grammar,datum)
        shouldRequest = isRecognitionGood and not isRecognitionMagical and grammar.should_request    # redundant for clarity
        print "should_request =", shouldRequest
        return shouldRequest

# you can abort any recognition by saying "abrogate". as a rare word, it will rarely be dictated (except for bootstrapping). if you must say it, it supports the "say" prefix (i.e. recognize literally everything after it). 
def handle_abrogation(data):
    return data[0] != "say" and "abrogate" in data 

# returns true if it matched the recognition (and executed the magic action).
# in which case, don't send a request to the server to execute any non-magic actions.
# "mike off" deactivates all grammars besides the microphone grammer, "putting the microphone to sleep".
def handle_microphone(grammar,datum):

    if  datum == "wake up" or datum == "mike on":
        # grammar.setMicState("on") 
        grammar.activateSet([microphone_export, H_EXPORT], exclusive=1)
        return True
    elif datum == "mike off":
        # grammar.setMicState("sleeping")
        grammar.activateSet([microphone_export],exclusive=1)
        return True
    elif datum == "mike dead":
        # the natlink.setMicState("off") # can't even be manually turned back on via the GUI
        return True
    else:
        return False

def handle_mode(grammar,datum):

#    print 'handle_mode(', datum, ')'

    if   datum == "dictating":
        grammar.activateSet([mode_export],exclusive=0)
        natlinkmain.recognitionMimic(["Dictation","mode"]) 
        grammar.should_request = False
        return True
    elif datum == "commanding":
        grammar.activateSet(all_exports,exclusive=1)
        grammar.should_request = True
        return True
    else:
        return False

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

def get_results(resultsObject):
    '''iterators are more idiomatic'''
    try:
        for number in xrange(10):
            yield resultsObject.getWords(number)
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
