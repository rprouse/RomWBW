�Z3ENV �LS� *	�)̀'�si+*S+0 "l+�] �/��$�  !E' ��e �  !M' ��ͪ&ͼ&ʶ$> *l+��&*l+"n+�9%(��2D'*l+ "n+�9%�J%*	� C ~�(���͞'��%
                           M A I N   M E N U


Installing  ��&�P'��%    1  Set default options
    2  Install editor
    3  User patch area

    S  Save changes
    X  Quit

 �q%��%X�$.�$�$SS$1v23� ��͞'��%
===== SET DEFAULT OPTIONS =================================


   1  Flag Options
   2  Numeric Options
   3  Character Options
   4  Time Display Options

   X  Main Menu �P'�q%��%1P253�4�X�.�� �͞'��%
----- FLAG OPTIONS ---------------------------------------��P'��%  0.     Recording On
  1.     Insert (vs. overwrite)
  2.     Auto sequence - SCREEN mode
  3.     Auto sequence - LINE mode
  4.     Start in SCREEN mode
  5.     String match forward (vs. reverse)
  6.     Show time in LINE mode prompt
  7.     Clear screen on entry to SCREEN mode
  8.     LINE mode - permit line concatenation with del left and del right
  9.     Line duplication command - replace first token with GO?
  A.     Line sequencing goes to next line (vs. same line)?

  X  Done
  R  Re-Start *n+ "Z+\+ ���O>�g�f��P'�q%��%0123&4.566>7F8N9VA^X�.��R� ��[Z+!\+ ���v&  �}& �}& �}& �}&	 �}&
 �}& �}& �}& �}&	 �}&
 �}.͹'!\+ Y~�>N(>Y��%.͹'!\+͏ͨ%��~/w�n'�r�v'͞'��%
----- TIME OPTIONS ---------------------------------------��P'��%  1.     Civil 12-hour time (vs. military 24-hour)
  2.     Use standout video mode for time display
  3.     Show time in line mode prompt
  4.     Time string separator character

  X  Done ��r� �r�'�r�.~��%�P'�q%��%1�2�3�4�Xv.vv ��ͨ%͏�� ��'��.�h*�(�w�n'~��%�v'�& .͹'*n+�& �& �& �͞'��%
----- NUMERIC OPTIONS ------------------------------------��P'��%  1.  Kbytes to read from log file
  2.  Screen mode text lines
  3.  Message time delay (1/10's)
  4.  Command line length to discard
  5.  Automatic shell pause line limit
  6.  Screen divider length

  X  Done   ͠%~�?�?�?w#6 �!-���%͹'�͓*$��P'�q%��%1�	2�3u4)56X�	.�	�	 �  ͠%~���w�v  Ͱ%�H��	&�Q�, *n+~��F�R��%KBYTES TO READ:  The more of the history file is read, the more is available
to use - but the slower the program load.  The default compromise is to
read the last 5k of the file.  The history may grow in size (to disk capacity)
without further slowing LSH.  The type 4 version may report "LOG OVFL" if the
buffer size is too large.  There should be room for 5-15k.
��"��%KBYTES TO READ:  Fixed Log version always reads and writes to a file of
fixed size.  Reserve outer tracks on your disk for fastest access.  File
size should depend upon disk's block size:  1,2, or 4k.
���%Kbytes to read from log:�̈́%�#r�`�:D'���u�͂~#foѯ�R"p+�:D'���u�[p+�͂s#r�*g+~#fo)��*n+ 	�" Ͱ%͘&�Q��%TEXT LINES:  The initial number of lines in the screen mode text window may
be set from a minimum of 1 up to the number of lines on your CRT (less 3).

Text Lines (1 to CRT): �Ä%�ͳ%��\Â	.-��%͹'�d'�n'*g+~͓*�v'�# Ͱ%̓&�Q��%TIME DELAY:  When some of LSH's functions are toggled, a display of the new
state is flashed.  The default setting is 4/10 second.

Message Delay (TENTHS): �Ä%$ Ͱ%�7&�Q��%MINIMUM SAVE:  Sets the line length LSH will discard rather than recording
to the log.  Default 0 saves everything.  A 1 would discard single character
command lines.

Line length to discard: �Ä%% Ͱ%�&	�Q��%SHELL PAUSE LIMIT:  The number of lines on display in the text window that
automatically cause a CLS and shell pause on command execution.  To force
a pause every time, set the limit to 0.  To disable this function, use 99.

Shell pause limit (LINES): �Ä%& Ͱ%�%&
�Q��%DIVIDER LENGTH:  In full screen mode, the log text appears between two divider
lines.  You can change the length of the divider.

Divider length (chars): �Ä%͞'��%
----- CHARACTER OPTIONS ----------------------------------��P'��%  1.  Log file type
  2.  Default command line
  3.  DU:DIR separator
  4.  Line mode prompt (SAVE ON)

  X  Done �P'#' ͠%��P'#x ͠%��P'#* ͠%��P'#+ ͠%~��P'�q%��%12g3�4Xv.vv �&
&&&.#��%͹'�d'�n'*g+��v'��~����%#��' Ͱ%�.ͳ%����%Log file default TYP: �>�z&~��ͨ%�[g+~�)+� �#��x Ͱ%�xͳ%����%DEFAULT COMMAND:  LSH will run a pre-installed command line on request.  The
default is HELPLSH, the help screen display program.  It should be installed
with a DIR reference so ZCPR33 can find it.  The command line may also be set
to run an alias or a useful program like ZFILER.

Default command line (16 char max):   >�z&͡&�ͨ%�V��* Ͱ%��ͳ%����%DU:DIR divider: �>�z&͡&�ͨ%�V+ Ͱ%�2"g+ͳ%���!.� !0��%��>�> ��%Line mode prompt    Save ON �*g+~���%    Save OFF �~�(==<���%
Reverse? (y/N)��b*�Y~ 
ͨ%�(==<w!.��!0�͞'��%
===== USER CODE PATCH =====================================


   1.  User Patch  (HEX)

         USR>


   X  Main Menu ͠�P'��%A small user's routine can be installed in the 32 byte patch area.
Code here is run when the User Patch key is pressed.  HEX code can be
entered directly, but it would be safer to assemble the patch and use
BCOMP to overlay it directly onto LSH.  The patch code location is at
ENTRY + 98h.  See LSH documentation for more information. �P'�q%��%1�X�.�� ��P'� ���P'�d'����P'��%Hex Bytes:�>a� ͠% ��͠ͨ%Æ͠%�d'*g+~�+��%�#���z&�*g+͡&��&�='�s �#�6  ����͞'��%
===== EDITOR INSTALLATION =================================


   1.  Editing keys
   2.  Word separator characters
   3.  Token separator characters

   I  Information
   X  Main Menu �P'�q%��%1�2�3�I X�.�� �͞'��%

                        COMMAND KEY INSTALLATION

     The control key bindings are listed in a table.  Letters represent
CONTROL KEYS entered while editing.  Those preceeded by a control character
are shifted keys scanned only after one of four lead-in keys has been pressed.

     To change a key binding, move the cursor to the function you want to
change.  To make it a shifted command, first enter a number 1-4 corresponding
to the lead-in key you want to assign.  Enter a character "A" through  "^"
for the base control key.  If you don't want a function implemented, enter
a "-" for it.

     For example, if lead-in key #3 is ^Q and you want to assign the control
sequence ^QZ to a function, first enter "3" then enter "Z".  The display then
shows the current key assignment.

     DEL is converted to ^_ by LSH, so that control key is not available.
 ��%Any key...  �h*�d Ͱ%
͙ͳ%����%Install characters recognized as word separators.  You can use no more
than the number below.  To use fewer, just repeat some of them.  Do not
start with a space.  [<CR> to skip installation]

Current   ->�>����%��%
Change to ->���z&�͡&�ͨ%�~������p Ͱ%ͭͳ%����%Install characters recognized as token separators for the token
replication command.  You can use no more than the number below.
To use fewer, just repeat some of them.  Do not start with a space.
[<CR> to skip installation]

Current   ->� >�o͞'��% Each command must have a unique key.  Letters represent control keys.  Enter
 a number 1-4 BEFORE entering the key to bind it to one of the lead-in keys.
 To disable a function use "-".  ESC CR DEL are entered directly.  TAB is "I".
 ��% Quit - ^C or "."     Skip - <sp>     ^E Up, ^D Rt, ^X Dn, ^S Lt (& Arrow Keys) �ͩ�� *n+- "U+*U+��� �7�2W+� ��:W+<�:W+� ��~#�-ʁ��*U+��(3��(�#��(�#��(�#�~ �@͵͵�[ ͒ͮ��%��_�@��_�@��%_�[�I�M� ��%ͼ���%DE����%ES����%TA����%C�����%������%������%������%�:W+G!�!�(~#������%����:Y+�g�KX+���o͹'��ɯ���:W+��0��G����CX+��� ���h*���%� 	� � � . � t � X f t � X f  �!�� � �� �� ͮ � ��� �� ͮ 0�� �� �� ͮ 0�� �� �� ͮ 0�� �� :Y+��� ͮ �� � �[W+ *U+��KX+��G:Y+�2W+�7ɯ2X+2Y+2W+�!Y+!X+4{��6 �!Y+!X+~�(57?�s7�ͨ%ͣ ^6 �18'�50#�0G�*U+�+�#= ��~ �-�='� >`� �w�h*�)+�-(��w��%   ��� �:W+�� �{�̩*U+���2W+�i!� � ͣ ~�-�:W+�<G>`� �_7*U+~�� �>7�2W+� ����%   ? �#��2W+�Lead-in Key #�Lead-in Key #�Lead-in Key #�Lead-in Key #�Execute Lin�Backspac�Char Lef�Char Righ�Line U�Line Dow�Word Righ�Word Lef�Line End/ Star�Line En�Line Star�Up Scree�Down Scree�First Scree�Last Scree�Delete Cha�Delete Lef�Delete Word R�Delete Word L�Delete Lin�Delete to EO�Delete Comman�New Lin�Insert/Overwrit�Unkill Deletio�Undo Line Chang�Control Ke�Clear to EO�Yank Line to �Yank & Kill Lin�Insert Q & Clea�Insert �Clear �Duplicate Lin�Old Line Searc�Toggle Directio�Recall Next Lin�Tog Line Recal�Toggle Sav�Quit Shel�Toggle Mod�Warm Boo�Shell Paus�Text Lines/ Tim�Repeat Token �Set Place Marke�Go to Next Marke�Clear Marker�Complete Filenam�HELP (Run Cmd�User Patc��Y$����%  Saving...�ͱ&ͼ& >*l+��&��&!�%ˆ�!�%�F ��%Save Changes? (Y/n)��b*�N�Y$�v'͞'͎'�{i+���% No fil���)6
>;)���%LSHINST - Default option and command key installation for LSH 1.1 
 Syntax:  LSHINST [dir:][lsh.com] î$ ��#����% Installs only LSH, Version 1.1  î$��%Choice: ��d'�b*>�z&͡&�����6 �I&�ͨ%*g+s�*n+"g+���2�%��͠%�P'ͳ'��d'��%��P'����%��~#���(�(��%����n'��v'�>��+����%������#(##� �#���^#V��  ~�#()~�)+� �� �#�08p�:8
�A8h�G0d��0�))))�o��#  ~�# #�~�)+� �� ��08<�:08�0#�bk)))�o|� g�����!� w��!� 6<�
�� �#^ #�6 �������~#� (�+��:i 2k+�!h �w#��:k+2i \ �*� <�\ � 2�&y������ ��&����� ���%File: �\ �B*x�A��%y͓*͜)�'��%��q*��%�~#� ��%��� 0���%���)+��%���%��� LSH     COM��V'���V#^#��͹'�����:y+*~+å'���[�+���[�+��(�͆'�����[�+�͔'�����[�+��(�*z+:w+�W~�(ͽ(������*�+�����[|+�(�((:x+��(�����������'�& �O�[�+�(��(��(��(:v+�g�:v+�g ��gɠg�(�������[�+�(��*(�%-���%(�\ �+��)+�D(L�2(C�3(;�.(+�+(*�>(�R(�I(�N ү�$,�}lg��0�g�|��+e�| |dͣ(
ͣ(�0��/�0��Gz�0 �A(�+x��~�(#�\ ~#�+�z�����O*�+|�(�+ �~�(G���(� �������(���� �����"�+|�(� "r+�|�(D~�!8?� ~2t+#~2u+#~2v+ w+ ��͋)��:t+O	��͋)� ��������)!w+6 #�s#r#�6 #s#r#�6 #�� �����s#r#�~#���\ �#���) ��~�(��# 	~� #���� 	������)###�w����*�+1 ����" *�+|�(~#fo�����)�� �0�x��� ��)*�+|�(^#V#~�������B*�(G! N�-*����"�+�����Y �Y*X�Y*���������Y*�� �Y*O�G������� �ѷ��h*�)+�>�4+������͇*>.�+͇*������ �+������&dͩ*&
ͩ*ͽ*����.��,0��g}� �|�> (��0G�+|������'��*������*d ��*
 ��*}�+��������R<0� ��> (��0G�+��F+�+���J+�+������O>�4+�����a��{��_����<+����o��* o���Ɛ'�@'ɮ+                                           