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
import natlink       # a DLL
import natlinkmain
import natlinkutils

# standard library
import time
import json
import urllib2
import traceback









# globals 

DEBUG = False # True 

#TODO hack, noise tends to be recognized as these short single words
noise_recognitions = set(["the",
                          "if",
                          "him",
                          "A",
                          "that",
                          "a",
                          "she", 
                         ])



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
print Numbers.TWO != int(Numbers.TWO)
# True 
print Numbers.TWO is Numbers.TWO
# True 
'''








# types 

Mode = enum("Mode", "Normal Correcting Dictating Sleeping Off Reading")

CorrectionStatus = enum("CorrectionStatus", "Success Heterophonic InvalidWord")

Microphone = enum("Microphone", "On Sleeping Off")








# Python helpers

def merge_dicts(*dictionaries):
    '''Given some dicts, merge them into a new dict as a shallow copy.'''
    d = dict()
    for dictionary in dictionaries: 
        d.update(dictionary)
    return d

# current time in milliseconds
def now():
    return int(time.clock() * 1000)

# http://stackoverflow.com/questions/1685221/accurately-measure-time-python-function-takes
def timeit(message, callback, *args, **kwargs):
    before = time.clock()
    result = callback(*args,**kwargs)
    after = time.clock()
    print message, ': ', (after - before) * 1000, 'ms'
    return result









# Dragon helpers 

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

# 
def from_dragon_recognition(words):
    '''
    >>> from_dragon_recognition(['spell', 'a\\\\spelling-letter\\\\A', ',\\\\comma\\\\comma', 'a\\\\determiner', 'letter'])
    ["spell", "A", ",", "a", "letter"]
    '''
    return list(decode_from_windows(word).split('\\\\')[0] for word in words)

# 
def into_dragon_correction(words):
    '''
    >>> into_dragon_correction [u'this', u'is', u'a', u'long', u'sentence'] 
    ['this', 'is', 'a', 'long', 'sentence']
    '''
    return list(encode_into_windows(word) for word in words)

# http://stackoverflow.com/questions/12468179/unicodedecodeerror-utf8-codec-cant-decode-byte-0x9c
def decode_from_windows(s): 
    return s.decode('cp1252').encode('utf-8')

def encode_into_windows(s): 
    return s.decode('utf-8').encode('cp1252')

def print_hypotheses(hypotheses):
    for (index, hypothesis) in enumerate(hypotheses):
        print "hypothesis %d = %s" % (index, from_dragon_recognition(hypothesis))

# the microphone state changes independently of the "command mode".  
def is_mode_awake(grammar):
    return not (grammar.current_mode in [Mode.Sleeping, Mode.Off])












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
dnsmode_rule = '''<dnsmode> exported = dictating | \{_modes_};'''

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
reading_rule = '''<reading> exported = \{_reading_} | \{_modes_} ;'''
reading_lists = dict(_reading_=["scroll", "scroll down", "scroll up"])

mode_lists = dict(_modes_=["speaking"])

active_exports =           [microphone_export, dnsmode_export, correctable_export, readable_export, H_EXPORT]
active_rules   = '\n'.join([microphone_rule,   dnsmode_rule,   correctable_rule,   readable_rule, H_RULES]) 

all_exports =           [microphone_export, dnsmode_export, correctable_export, correcting_export, reading_export, readable_export, H_EXPORT]
all_rules   = '\n'.join([microphone_rule,   dnsmode_rule,   correctable_rule,   correcting_rule,   reading_rule,   readable_rule, H_RULES]) 
all_lists   = merge_dicts(H_LISTS, reading_lists, mode_lists)

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
    current_mode = Mode.Normal 


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
        words = from_dragon_recognition(raw)

        print 'raw   =', raw
        if DEBUG: print 'words =', words

        try:

            # 
            should_send_request = try_magic_handlers(self,words)
            if not should_send_request: 
                self.should_request = False
            else: 
                self.should_request = should_request_in_mode(self.current_mode)  

            # we set it only the first time we enter "correcting" mode 
            # i.e. we don't want to try to "correct the correction"
            if self.current_mode == Mode.Correcting and self.correcting_results == None: 
                self.correcting_results = self.previous_results
                # TODO must enable requests to pass through, after the first "fix-it" is ignored 
                results_identity = id(self.correcting_results)
                raw_hypotheses = list(get_results(self.correcting_results))
                print_hypotheses(raw_hypotheses)
                hypotheses = [from_dragon_recognition(hypothesis) for hypothesis in raw_hypotheses] 
                (_, data) = post_hypotheses(results_identity, hypotheses)
                handle_response(self, data)

            # 
            self.previous_results = resultsObject

            # the microphone falls asleep during silence,
            # but doesn't wake up when a command is uttered. 
            # we turn the microphone back on, as long as it should be on. 
            if is_mode_awake(self):
                set_microphone(Microphone.On)

            # the microphone may have been toggled manually by the GUI,
            # or fallen asleep automatically. 
            if self.current_mode == Mode.Sleeping or self.current_mode == Mode.Off: 
                if get_microphone() == Microphone.On: 
                    should_change_microphone_mode = handle_microphone(self,"mike on")
                    if should_change_microphone_mode:
                        self.current_mode = should_change_microphone_mode
                if get_microphone() == Microphone.Sleeping: 
                    should_change_microphone_mode = handle_microphone(self,"mike off")
                    if should_change_microphone_mode:
                        self.current_mode = should_change_microphone_mode

            print "current_mode   =", self.current_mode
            print "should_request =", self.should_request

            response = None 
            if self.should_request:
                (response, data) = post_recognition(words, self.current_mode) 
                handle_response(self, data)

        except Exception as e:
            print
            print "---------- error ------------------"
            print "sending the request and/or handling the response threw:"
            print e
            print traceback.format_exc()

        # don't print until the request is sent the response is handled
        try:
            print
            if response: 
                print "status  =", response.getcode()
            print "previous_results   =", first_result (self.previous_results) 
            print "correcting_results =", first_result(self.correcting_results) 

        except NameError as e:
            print 
        except Exception as e:
            print
            print "---------- error ------------------"
            print e
            print traceback.format_exc()


    # for debugging only, shows whether specific rules (rather than the generic dgndictation) are matching the recognition
    # only called when (self.doOnlyGotResultsObject=False)
    # def gotResults(self, words, fullResults):


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
    data = json.load(response) 
    if DEBUG: print 'data  =', data 
    return (response, data) 

# hypotheses :: [[String]] 
def post_hypotheses(identifier, hypotheses):
    url      = "%s/hypotheses/" % (server_address,)        # TODO parameterize API
    # data     = json.dumps([identifier, hypotheses])
    data     = json.dumps(hypotheses)
    request  = urllib2.Request(url, data, \{"Content-Type": "application/json"})
    print 'url   =', url
    print 'data  =', data 
    response = urllib2.urlopen(request)
    data = json.load(response) 
    if DEBUG: print 'data  =', data 
    return (response, data) 

# 
def handle_response(self, data):

    x = validate_correction(data) 
    if x: 
        (identifier, correctedRecognition_Unicode) = x
        correctedRecognition_Windows = into_dragon_correction(correctedRecognition_Unicode) 
        originalRecognitionObject = self.correcting_results
        correctionStatus = perform_correction(originalRecognitionObject, correctedRecognition_Windows)
        print "correction_status      =", correctionStatus  
        set_mode(self, Mode.Normal)            # TODO NOTE only this should change away from correcting mode 
        self.correcting_results = None 
        return correctionStatus

# e.g. data = \{\"_responseCorrection\":[0,[\"some\",\"words\"]],\"_responseMicrophoneState\":null,\"_responseDNSMode\":null}
def validate_correction(data):
    try: 
        if DEBUG: print "VALIDATING CORRECTION" 
        (identifier, correction) = data["_responseCorrection"] 
        return (identifier, correction) 
    except (ValueError, IndexError, TypeError) as e:  
        if DEBUG: print "  error = " , e 
        return None 





# magic 

# data :: [String] 
# datum :: String
def try_magic_handlers(grammar,words):
    data = tuple(words)       # tuples are immutable and thus hashable 
    datum = " ".join(data)

    # truthy if the recognition is good enough
    isGood = is_recognition_good(data, datum) 

    if isGood:
        didPerformMagic = handle_everything(grammar,datum)
        shouldSendRequest = not didPerformMagic 
        return shouldSendRequest 

    else:
        return False 


# 
def is_recognition_good(data, datum): # TODO speed up 
    return (    True
            and     data 
            and     is_unicode(data)
            and not is_noise(datum)
            and not is_abrogation(data)
           )


# you can abort any recognition by saying "abrogate". as a rare word, it will rarely be dictated (except for bootstrapping). if you must say it, it supports the "say" prefix (i.e. recognize literally everything after it). 
def is_abrogation(data):

    # TODOthe first predicate ("say") is a hack to let you say the magic word ("abrogate")
    # helps with development 
    # order for efficiency (LOL) 
    return data[0] != "say" and "abrogate" in data


# 
def is_unicode(data):
    try:
        for word in data:
            word.decode('utf8')
        return True
    except UnicodeDecodeError as e:
        print e
        print traceback.format_exc()
        return False


# 
def is_noise(data):
    return data in noise_recognitions   


# 
# truthy if the recognition has been handled "magically"
# by the client, and shouldn't be handled by the server TODO make this API explicit in servant
def handle_everything(grammar,datum):
    return (   False 
            or handle_microphone(grammar,datum) 
            or handle_dnsmode(grammar,datum)
            or handle_correctable(grammar,datum)
           )


# 
def handle_dnsmode(grammar,datum):

    # print 'handle_dnsmode(', datum, ')'

    if   datum == "speaking":
        set_mode(grammar, Mode.Normal ) 
        return True 

    elif datum == "dictating":
        set_mode(grammar, Mode.Dictating ) 
        return True 

    elif datum == "reading":
        set_mode(grammar, Mode.Reading ) 
        return True 

    else:
        return False 


# returns true if it matched the recognition (and executed the magic action).
# in which case, don't send a request to the server to execute any non-magic actions.
# "mike off" deactivates all grammars besides the microphone grammer, "putting the microphone to sleep".
def handle_microphone(grammar,datum): # TODO 

# def read_microphone(datum):   
    if  datum == "wake up" or datum == "mike on":
        set_mode(grammar, Mode.Normal ) 
        return True 

    elif datum == "go to sleep" or datum == "mike off":
        set_mode(grammar, Mode.Sleeping ) 
        return True 

    elif datum == "mike dead":
        set_mode(grammar, Mode.Off ) 
        return True 

    else:
        return False  


# 
def handle_correctable(grammar,datum):

    if   datum == "fix-it" or datum == "correct":
        set_mode(grammar, Mode.Correcting ) 
        return True 

    else:
        return False

# NOTE expects Dragon encoding. shouldn't throw.   
def perform_correction(originalRecognitionObject, correctedRecognition): 
    try: 
        perform_correction_logging(originalRecognitionObject, correctedRecognition) 
        status = originalRecognitionObject.correction(correctedRecognition) 
        return CorrectionStatus.Success if status else CorrectionStatus.Heterophonic
    except natlink.InvalidWord: 
        return CorrectionStatus.InvalidWord 

def perform_correction_logging(originalRecognitionObject, correctedRecognition): 
    print "original_recognition   =", first_result(originalRecognitionObject) 
    print "corrected_recognition  =", correctedRecognition 
    return None 

# 
def set_mode(grammar, mode):

    if   mode == Mode.Normal :
        grammar.activateSet(active_exports,exclusive=1)

    elif mode == Mode.Correcting : 
        grammar.activateSet([correcting_export],exclusive=1)

    elif mode == Mode.Dictating : 
        grammar.activateSet([dnsmode_export],exclusive=0)     # NOTE nonexclusive, enables dragons built-in commands 
        natlinkmain.recognitionMimic(["Dictation","mode"]) 

# TODO check audio 

    elif mode == Mode.Sleeping : 
        set_microphone(Microphone.Sleeping) 
        grammar.activateSet([microphone_export],exclusive=1)

    elif mode == Mode.Off : 
        set_microphone(Microphone.Off)
        grammar.activateSet([microphone_export],exclusive=1)

    elif mode == Mode.Reading : 
        grammar.activateSet([reading_export],exclusive=1) 

    else: 
        raise TypeError("set_mode", mode) 

    grammar.current_mode = mode  
    return mode 

# 
def set_microphone(state):
    if   state == Microphone.On: 
        natlink.setMicState("on") 

    elif state == Microphone.Sleeping: 
        natlink.setMicState("sleeping")

    elif state == Microphone.Off: 
        natlink.setMicState("off")

    else: 
        raise TypeError("set_microphone", state) 

# 
def get_microphone():
    s = natlink.getMicState()
    if   s == "on":
        return Microphone.On 
    elif s == "sleeping":
        return Microphone.Sleeping 
    elif s == "off":
        return Microphone.Off 
    else: 
        raise TypeError("get_microphone", s) 

# 
def should_request_in_mode(mode): 
    if   mode in [Mode.Normal, Mode.Correcting, Mode.Reading]: 
        return True 
    elif mode in [Mode.Dictating, Mode.Sleeping, Mode.Off]: 
        return False 
    else: 
        raise TypeError("should_request_from_mode", mode)








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
