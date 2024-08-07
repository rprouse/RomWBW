�Z3ENV  SALIAS�*	͚�$�s�*�0 " �] �/ʭ�  !� ��e �  !� ����1ʕ* d �L* ��."�[ ��R�%�l& "�*	� =
~�(����6��
                         SALIAS 1.5 INSTALLATION


Installing  �q����    1  Install Editing Keys
    2  Install Word Separator Chars
    3  Install Insert/Overwrite Flag

    I  Information

    S  Save Changes
    X  Quit

 ͅ��X^.^S4I�1�2�3< ���6��

                        COMMAND KEY INSTALLATION

     The control key bindings are listed in a table.  Letters represent
CONTROL KEYS entered while editing.  Those preceeded by a control character
are shifted keys scanned only after one of four lead-in keys has been pressed.

     To change a key binding, move the cursor to the function you want to
change.  To make it a shifted command, first enter a number 1-3 corresponding
to the lead-in key you want to assign.  Enter a character "A" through  "^"
for the base control key.  If you don't want a function implemented, enter
a "-" for it.

     For example, if lead-in key #2 is ^Q and you want to assign the control
sequence ^QZ to a function, first enter "2" then enter "Z".  The display then
shows the current key assignment.

     DEL is converted to ^_ by SALIAS, so that control key is not available.

 ��Strike Any key --  �
Õ
 ͭ�KͰ����Default Insert On?   ~͉��  -->  �
���Y(�N��w�=wͤɷ>Nʽ>Yý5 ͭͣͰ����Install characters recognized as word separators.  You can use no more
than the number below.  To use fewer, just repeat some of them.  Do not
start with a space.  [<CR> to skip installation]

Current   ->  �~ͽ#���
Change to ->  >�����ͤ�~�������6�� Each command must have a unique key.  Letters represent control keys.  Enter
 a number 1-3 BEFORE entering the key to bind it to one of the lead-in keys.
 To disable a function use "-".  ESC CR DEL are entered directly.  TAB is "I".

 Quit - ^C or "."     Skip - <sp>     ^E Up, ^D Rt, ^X Dn, ^S Lt (& Arrow Keys) �ͬ͗
* "�*���	�
�)�2�� 	;	:�<�	:��
��	~#�-�{	��*���(-��(�#��(�#�~ �@ʹ	ʹ	�[ ͍	ͬ	ͽ��_�@��_�@��_�	[�	I�	M�	 ͽ͹	���DEL ���ESC ���TAB ���CR ��>^ͽ���>��> ��͹	:�G!�(~#���	�������:��g�K���	�o�Q��ɯ���:���0��G���	�C��͗
��	�
����
	�
�
�
.�� j
\
a
R
W
\
a
R
W
 �!��
�
ͩ
ͼ
͵
͢
̓
�
͢
:�� �ͩ
��[� *���K���	G:��2��)د2�2�2��!�!�4{��6 �!�!�~�(5�s�ͤ�x
^6 �18'�50#�0G�*��+�#= ��~ �-̹�
>`� �w�
���-(��w��     �� 	�:���
�{�̬*���	�2��<�
�
�x
~�-�:��<G>`� �_)*�~�� �>)�2��
��	��   ? �#��2��Lead-in Key #�Lead-in Key #�Lead-in Key #�Backspac�Cursor L�Cursor R�Cursor U�Cursor D�Word R�Word L�End/Start of Lin�Line Star�Line En�Up Scree�Down Scree�Delete Cha�Delete Char L�Delete Word R�Delete Lin�Delete to EO�Carriage Retur�Insert Lin�Indent Lin�Indent Scrip�Insert/Overwrit�Control Cha�Fin�Replac�Repeat Find/Re�Clear (Zap�Save & Resum�Save & Clea�Save & Qui�Qui�FILE Mod�Renam�Read Fil�Toggle Mod�Undo Change�Print Scrip�Hel��:����  Saving...  �&�1*�d�D�2��:��(#��Save Changes? (Y/n)  �
���N�:��6�,�{���� No file �n6
>�V���SAINST - Default option and command key installation for SALIAS 1.5
  Syntax:  SAINST [salias.com]
 Í�d 	�>�W������� Installs only SALIAS, Version 1.5 Í�����B ����G��x(
�#�	�0�=������# ������Choice:   ��
��*"����=2��� ͜���������~#��ͽ���>ý����#(##� �#���^#V����!� w��!� 6<�
�� �#^ #�6 �������~#� (�+��:i 2��!h �w#��:�2i \ ͕� <�\ � y�������F����� ���"*�"} ���File:   \ ��x�Aͽy�5�4ě>:ͽ�>/ͽ~#� Ľ��� 0ͬ	��ý�>ͽ��SALIAS  COM�����������~#���(	�(
ͽ�����������V#^#���Q�����:*�=���[���[�÷���[���[�÷�*:	�W~�(�U������*�����[�(��:
�f����������s�& �O�["̛�ܛ�ܛ�ܛ:�g�:�g ��gɠgͰ�������[ �(����%-���%(�\ ͽ����D(L�2(C�3(;�.(+�+(*�>(�R(�I(�N ү�$,�}lg��0�g�|�ͽe�| |d�;
�;�0��/�0��Gz�0 �A(ͽx��~�(#�\ ~#ͽ�z�����O*>|�(�+ �~�(G�͌� �����͒���� �����">|�(� "�|�(D~�!8?� ~2#~2#~2 	 ���#��:O	���#� �������!	6 #�s#r#�6 #s#r#�6 #�� �����s#r#�~#���\ �#��} ��~�(��# 	~� #���� 	�����n###�w����*>1 ����" *>~#fo���� Ä*>|�(^#V#~�ѷ�������(G! NͲ����">�����Y ��X�������������� ��O�G�����>��������� �ѷ����O� ������>ͽ>
ͽ���>���������)>.ͽ�)������ Ľ������&d�K&
�K�_����.��,0��g}� �|�> (��0Gͽ|������'͓����͓d ͓
 ͓}ͤ��������R<0� ��> (��0Gý���ͽ����ͽ�����O>�������a��{��_���������o��* o���Ɛ'�@'�@            