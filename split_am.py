#! /usr/bin/python
import sys, os

DEFAULT_FILENAME = 'amfiles.txt' 
# default name of file containing list of filenames

def mainFunction(filename):
    ''' Main function '''
    # Check if filename points to existing file
    if not os.path.isfile(filename):
        print 'Input file "%s" not found'%(filename)
        sys.exit(2) 
# program stops with return code 2 (or whichever error code you decide...)
    
    # Read list of files from input file
    for line in open(filename):
       f_name = line.strip() 
# filename written on that line (strip removes spaces before and after)
       separating(f_name)

def separating(filename):
    infile = open(filename + '.am', 'r')
    print "Input file:", filename + '.am'
    outfile1 = open(filename + '.header', 'w')
    print "Output header file:", filename + '.header'
    outfile2 = open(filename + '.binary','wb')
    print "Output binary file:", filename + '.binary'
    n = 0
    while n != 1:
        txt = infile.readline()
        outfile1.write(txt)
        if txt == "@1\n":
            n = 1
            print"ASCII Header END = @1"
#           print"ASCII Header END = @1", txt
        else:
            n = 0           

    data_bin = infile.read(-1)
    data_len = len(data_bin)
    num_bytes = data_len
    print "Number of data bytes is: ",num_bytes
    outfile2.write(data_bin)
    print " "

if __name__ == "__main__":
    # Try getting filename from command line, otherwise use default
    if len(sys.argv) > 1:
        filename = sys.argv[1]
        f_name = filename 
        separating(f_name)
    else:
        filename = DEFAULT_FILENAME
        print 'Running program on input file "%s"'%(filename)
        print " "
        mainFunction(filename)

    print "Done."

