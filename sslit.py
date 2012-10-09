#>
# Bare literate programming tool
# ==============================
#<

import sys

#>
# This is an implementation of a bare literate programming tool.
# This literate programming style doesn't support macros not file
# generation.
# 
# To cut a long story short, this literate programming tool extracts
# MarkDown-style comments from a source code. The code, instead, is
# formatted as a preformatted block.
# 
# To recognize markdown-documentation from source code the documentation
# must came before a particular "documentation start" marker and must 
# be followed by a particular "documentation end" marker.
#
# A documentation start line must:
# 
# * be long 3 characters or less;
# * end with a `>` sign.
#<

def isDocumentationStart(line):
	return len(line)<=3 and line.endswith(">")

#>
# A documentation end line must:
#
# * be long 3 characters or less;
# * end or start with a `<` sign.
#<

def isDocumentationEnd(line):
	return len(line)<=3 and (line.endswith("<") or line.startswith("<"))

#>
# Documentation extractor
# -----------------------
#
# The documentation extractor is implemented as a simple state machine
# which can be in code-status or in documentation-status. Every state
# machine status can output the extracted documentation:
#<

class Status(object):
  def output(self, s):
    print s

#>
# ### Code status
#
# The code status outputs, before the source code, 4 spaces. This is the
# MarkDown syntax for a code block. The first line of a code block is used
# to calculate a common prefix for all the lines of the code block.
#
# This renders correctly source code with a common indentation level.
#<

class CodeStatus(Status):
  def __init__(self):
    self._firstLine = True

  def process(self, row):
    if self._firstLine:
      self._firstCol = len(row) - len(row.lstrip())
      self._firstLine = False
      row = row.lstrip()

    elif len(row)>=self._firstLine:
      row = row[self._firstCol:]

    self.output("    " + row)

#>
# ### Documentation status
# 
# The documentation status simply output the documentation as-is without
# a common prefix (if exists):
#<

class TextStatus(Status):
  def __init__(self):
    self._firstLine = True
    self._prefix = None

  def process(self, row):
    if self._firstLine:
      self._firstLine = False
      self._prefix = row[:-1]
      row = row.lstrip()

    elif len(row)>=self._firstLine:
      if row.startswith(self._prefix):
        row = row[len(self._prefix)+1:]

      self.output(row)

#>
# ### State machine
#
# The current status is changed when the program see a documentation start/end line:
#< 

def process(fileName):
  status = CodeStatus()

  for row in open(fileName):
    row = row.rstrip()
    row = tabToSpaces(row)

    if isDocumentationStart(row.strip()):
      status = TextStatus()
      status.process(row)
    elif isDocumentationEnd(row.strip()):
      status = CodeStatus()
    else:  
      status.process(row)

#>
# Utilities
# ---------
#
# To calculate the common prefix of a documentation block the source code lines
# are considerated with tabs expanded to 4 spaces:
#<

def tabToSpaces(row):
  result = ""
  while len(row)>0 and row[0]=='\t': 
    result += "    "
    row = row[1:]
  result += row
  return result

#>      
# Starting point
# --------------
#
# The script file implements only a file filter: it takes an input file parameter
# and write, on the standard output device, the extracted documentation:
#<

def main():
  if len(sys.argv)!=2:
    print "Use:", sys.argv[0], "<in>"
    return
  else:
    process(sys.argv[1])

if __name__=="__main__": main()    

