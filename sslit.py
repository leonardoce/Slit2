#>
# This is an implementation of a bare literate programming tool.
# This literate programming style doesn't support macros not file
# generation.
#<
import sys

def tabToSpaces(row):
  result = ""
  while len(row)>0 and row[0]=='\t': 
    result += "    "
    row = row[1:]
  result += row
  return result

class Status(object):
  def output(self, s):
    print s

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

def process(fileName):
  status = CodeStatus()

  for row in open(fileName):
    row = row.rstrip()
    row = tabToSpaces(row)

    if len(row.strip())<=3 and row.endswith(">"):
      status = TextStatus()
      status.process(row)
    elif len(row.strip())<=3 and row.lstrip().startswith("<"):
      status = CodeStatus()
    else:  
      status.process(row)

def main():
  if len(sys.argv)!=2:
    print "Use:", sys.argv[0], "<in>"
    return
  else:
    process(sys.argv[1])

if __name__=="__main__": main()    
