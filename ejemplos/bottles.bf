##########################
###
### Severely updated version!
### (now says "1 bottle" and
### contains no extra "0" verse)
###
##########################
### 99 Bottles of Beer ###
### coded in Brainfuck ###
### with explanations  ###
##########################
#
# This Bottles of Beer program
# was written by Andrew Paczkowski
# Coder Alias: thepacz
# three_halves_plus_one@yahoo.com
#####

>                            0 in the zeroth cell
+++++++>++++++++++[<+++++>-] 57 in the first cell or "9"
+++++++>++++++++++[<+++++>-] 57 in second cell or "9"
++++++++++                   10 in third cell
>+++++++++                    9 in fourth cell

##########################################
### create ASCII chars in higher cells ###
##########################################

>>++++++++[<++++>-]               " "
>++++++++++++++[<+++++++>-]        b
+>+++++++++++[<++++++++++>-]       o
++>+++++++++++++++++++[<++++++>-]  t
++>+++++++++++++++++++[<++++++>-]  t
>++++++++++++[<+++++++++>-]        l
+>++++++++++[<++++++++++>-]        e
+>+++++++++++++++++++[<++++++>-]   s
>++++++++[<++++>-]                " "
+>+++++++++++[<++++++++++>-]       o
++>++++++++++[<++++++++++>-]       f
>++++++++[<++++>-]                " "
>++++++++++++++[<+++++++>-]        b
+>++++++++++[<++++++++++>-]        e
+>++++++++++[<++++++++++>-]        e
>+++++++++++++++++++[<++++++>-]    r
>++++++++[<++++>-]                " "
+>+++++++++++[<++++++++++>-]       o
>+++++++++++[<++++++++++>-]        n
>++++++++[<++++>-]                " "
++>+++++++++++++++++++[<++++++>-]  t
++++>++++++++++[<++++++++++>-]     h
+>++++++++++[<++++++++++>-]        e
>++++++++[<++++>-]                " "
++>+++++++++++++[<+++++++++>-]     w
+>++++++++++++[<++++++++>-]        a
>++++++++++++[<+++++++++>-]        l
>++++++++++++[<+++++++++>-]        l
>+++++[<++>-]                      LF
++>+++++++++++++++++++[<++++++>-]  t
+>++++++++++++[<++++++++>-]        a
+++>+++++++++++++[<++++++++>-]     k
+>++++++++++[<++++++++++>-]        e
>++++++++[<++++>-]                " "
+>+++++++++++[<++++++++++>-]       o
>+++++++++++[<++++++++++>-]        n
+>++++++++++[<++++++++++>-]        e
>++++++++[<++++>-]                " "
>++++++++++[<++++++++++>-]         d
+>+++++++++++[<++++++++++>-]       o
++>+++++++++++++[<+++++++++>-]     w
>+++++++++++[<++++++++++>-]        n
>++++++++[<++++>-]                " "
+>++++++++++++[<++++++++>-]        a
>+++++++++++[<++++++++++>-]        n
>++++++++++[<++++++++++>-]         d
>++++++++[<++++>-]                " "
++>+++++++++++[<++++++++++>-]      p
+>++++++++++++[<++++++++>-]        a
+>+++++++++++++++++++[<++++++>-]   s
+>+++++++++++++++++++[<++++++>-]   s
>++++++++[<++++>-]                " "
+>+++++++++++++[<++++++++>-]       i
++>+++++++++++++++++++[<++++++>-]  t
>++++++++[<++++>-]                " "
+>++++++++++++[<++++++++>-]        a
>+++++++++++++++++++[<++++++>-]    r
+>+++++++++++[<++++++++++>-]       o
>+++++++++++++[<+++++++++>-]       u
>+++++++++++[<++++++++++>-]        n
>++++++++++[<++++++++++>-]         d
>+++++[<++>-]                      LF
+++++++++++++                      CR

[<]>>>>      go back to fourth cell

#################################
### initiate the display loop ###
#################################

[            loop
 <           back to cell 3
 [            loop
  [>]<<       go to last cell and back to LF
  ..          output 2 newlines
  [<]>        go to first cell

 ###################################
 #### begin display of characters###
 ###################################
 #
 #.>.>>>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>
 #X X     b o t t l e s   o f   b e e r  
 #.>.>.>.>.>.>.>.>.>.>.>.
 #o n   t h e   w a l l N
 #[<]>    go to first cell
 #.>.>>>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>>>>>>>>>>>>>.>
 #X X     b o t t l e s   o f   b e e r             N
 #.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>
 #t a k e   o n e   d o w n   a n d   p a s s   
 #.>.>.>.>.>.>.>.>.>.
 #i t   a r o u n d N
 #####

  [<]>>      go to cell 2
  -          subtract 1 from cell 2
  <          go to cell 1

 ########################
 ### display last line ##
 ########################
 #
 #.>.>>>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>
 #X X     b o t t l e s   o f   b e e r  
 #.>.>.>.>.>.>.>.>.>.>.
 #o n   t h e   w a l l
 #####

  [<]>>>-      go to cell 3/subtract 1
 ]            end loop when cell 3 is 0
 ++++++++++   add 10 to cell 3
 <++++++++++  back to cell 2/add 10
 <-           back to cell 1/subtract 1
 [>]<.        go to last line/carriage return
 [<]>         go to first line

########################
### correct last line ##
########################
#
#.>.>>>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>
#X X     b o t t l e s   o f   b e e r  
#.>.>.>.>.>.>.>.>.>.>.
#o n   t h e   w a l l
#####

 [<]>>>>-    go to cell 4/subtract 1
]           end loop when cell 4 is 0

##############################################################
### By this point verses 99\10 are displayed but to work   ###
### with the lower numbered verses in a more readable way  ###
### we initiate a new loop for verses 9\0 that will not    ###
### use the fourth cell at all                             ###
##############################################################

+           add 1 to cell four (to keep it non\zero)
<--         back to cell 3/subtract 2

[            loop
 [>]<<       go to last cell and back to LF
 ..          output 2 newlines
 [<]>        go to first cell

 ###################################
 #### begin display of characters###
 ###################################
 #
 #>.>>>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>
 # X     b o t t l e s   o f   b e e r  
 #.>.>.>.>.>.>.>.>.>.>.>.
 #o n   t h e   w a l l N
 #[<]>    go to first cell
 #>.>>>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>>>>>>>>>>>>>.>
 # X     b o t t l e s   o f   b e e r             N
 #.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>
 #t a k e   o n e   d o w n   a n d   p a s s   
 #.>.>.>.>.>.>.>.>.>.
 #i t   a r o u n d N
 #####

 [<]>>       go to cell 2
 -           subtract 1 from cell 2

 ########################
 ### display last line ##
 ########################
 #
 #.>>>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>
 #X     b o t t l e s   o f   b e e r  
 #.>.>.>.>.>.>.>.>.>.>.
 #o n   t h e   w a l l
 #####

 [<]>>>-     go to cell 3/subtract 1
]            end loop when cell 3 is 0
+            add 1 to cell 3 to keep it non\zero

[>]<.        go to last line/carriage return
[<]>         go to first line

########################
### correct last line ##
########################
#
#>.>>>.>.>.>.>.>.>.>>.>.>.>.>.>.>.>.>.>
# X     b o t t l e    o f   b e e r  
#.>.>.>.>.>.>.>.>.>.>.<<<<.
#o n   t h e   w a l l
#####

[>]<<       go to last cell and back to LF
..          output 2 newlines
[<]>        go to first line

#########################
### the final verse    ##
#########################
#
#>.>>>.>.>.>.>.>.>.>>.>.>.>.>.>.>.>.>.>
# X     b o t t l e    o f   b e e r  
#.>.>.>.>.>.>.>.>.>.>.>.
#o n   t h e   w a l l N
#[<]>        go to first cell
#>.>>>.>.>.>.>.>.>.>>.>.>.>.>.>.>.>.>>>>>>>>>>>>>.>
# X     b o t t l e    o f   b e e r             N
#.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>
#t a k e   o n e   d o w n   a n d   p a s s   
#.>.>.>.>.>.>.>.>.>.
#i t   a r o u n d N
#[>]<        go to last line
#<<<.<<.<<<.
#   n  o    
#[<]>>>>     go to fourth cell
#>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>.>
#   b o t t l e s   o f   b e e r  
#.>.>.>.>.>.>.>.>.>.>.>.
#o n   t h e   w a l l N
#####fin##