import io,sys,string
import re

def consumeFile(filename, lineProcessor=(lambda l: sys.stdout.write(l + "\n"))):
    """ Reads the lines of a file, presenting each to the line processor, after
        stripping superfluous whitespace, esp. the terminating newline character
    """
    with open(filename, 'r') as f:
        for line in f:
            lineProcessor(string.strip(line))


def regexpParser(l, regexp, tokenConsumer):
    """ A line processor that applies a regular expression to the line received,
        then sends the tokens to a tokenConsumer function. If the line does
        not match, None is send """
    m = regexp.match(l)
    if m:        
        return tokenConsumer(m.groups())
    return None

def buildRebelFileProcessor(consumer):
    """ Builds a Rebel-specific file-processing function - this
        function applies a special regular expression that extracts
        name and login, using the supplied consumer to handle the
        strings it has found. Note that this actually builds a closure
        - it assembles the information neccessary and puts it into a
        function definition. The resulting function will thus work on
        the proper regular expression and use the provided processor.
    """
    regexp = re.compile(r'([mf\?])\|\|\[\"([^\"]*)\".*?(\w+)</span>\)\]\]\|\|.*?(\w+)</span>\)\]\]\|\|$')

    def rebelParser(l):
        """ This method more or less acts as a currying: it binds two of three 
            parameters...
        """
        regexpParser(l, regexp, consumer)

    return rebelParser

def successing(*fns):
    """ A method that runs functions in succession. Used for showing off.  """
    for f in fns:
        f()

def sqlOutputConsumer(groups):
    """A processor that will output the received groups as an SQL insert string
       fitting for our Netlighters database """
    if groups:
        print "INSERT INTO Netlighters (name, login) VALUES ('%s', '%s');"%(groups[1], groups[3])
        print "INSERT INTO Answers (question_id, netlighter_id, content) VALUES (1, currval('netlighters_seq'), '%s');"%(groups[2])
        if groups[0] == 'm':
            print "INSERT INTO Answers (question_id, netlighter_id, content) VALUES (2, currval('netlighters_seq'), 'male');"
        if groups[0] == 'f':
            print "INSERT INTO Answers (question_id, netlighter_id, content) VALUES (2, currval('netlighters_seq'), 'female');"

""" This is the main call, consuming the file provided by the user with a 
    rebel file processor that uses a sqlOutputConsumer to consume the tokens
    parsed from the file. If that ain't functional!"""
successing(lambda : consumeFile('createdb'), lambda : consumeFile(sys.argv[1], buildRebelFileProcessor(sqlOutputConsumer)), lambda : consumeFile('fixdb'))



