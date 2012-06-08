
                                Fonzie
                                ======

Fonzie is a program which attempts to identify cameras in catalog
listings.  It is my entry in Sortable's coding challenge at

    http://sortable.com/blog/coding-challenge/

It requires two input files containing a list of products and listings
respectively, formatted as specified in the challenge.  Example data
files are available at the website above.


Running Fonzie
==============

To run Fonzie:

    0) Get a recent Linux system with recent Scala development tools
       and GNU make. (I use XUbuntu 12 and Scala 2.9.2 but anything
       sufficiently recent should work.)

    1) Clone the repository and 'cd' into it.

    2) Type 'make'.

    3) Assuming your data files are in the parent directory, type:

        scala Fonzie.jar ../products.txt ../listings.txt match.txt reject.txt

       Adjust your filenames according to taste.

       The first two parameters are the input files (products and
       listings respectively), the third is the name of the file that
       will contain the results and the fourth (optional) contains the
       rejected listings.
 

Matching Threshold
==================

Fonzie takes one optional parameter, '--threshold', followed by a
floating-point constant between 0.0 and 1.0.  This is the minimum
matching threshold.  It is set to 0.5 by default.

For each possible listing/product pair, Fonzie computes a score
between 0 and 1 to represent how likely they are to match, then
selects the pair with the highest score.  If the score is lower than
the threshold however, it is rejected as unmatchable.

Raising or lowering the threshold can adjust the false-negative/
false-positive balance but does not affect how listings are associated
with products.

For example:

    scala Fonzie.jar --threshold 0.8  products.txt listings.txt \
        matches.txt rejects.txt
