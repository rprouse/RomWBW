�"Z3ENV   INIRAMD  M-RAM    000* ��"	��
͑�?� 4�u	 Initialize B/P RAM Drive   V1.1   12 Apr 93
 �s[1[�T�C:] �/ :^ �/ʧ* .Z~�� �>�C!��~�B #~�/ #~�P(-�u	
+++ Not B/P Bios ... aborting +++
 ��:\ �(�@2:�AO �,|� ��u	does NOT Exist ! "++~�(7��u	is Not a RAM Drive ! �t�x:! �A()= ��%� � ^#V��+++~2�^#V�S��2##~#<2�(� ~�(�2�2###^#V#�:��:�{2�?2V#^��)0<�2� ~2��MD�5:2!m �f!] �f�? *�u	
  Are you Sure? (Y/[N]) :  ́�t�Y b�t!\ ͹��:�AG �?\ � <(`�u	
+++ Already Formatted...Proceed anyway (Y/[N]):  ́�Y�t(�u	 ...aborting...
 �K�?�  ͒���:�(3�!��� 	:O�?0<G q##:���8�_�{2���  ���͹��5͒�?� $�u	Track  :͖�u	  Sector  :͖͌!5(��u	

...Drive   :���u	  is Initialized :�(�u	
     (P2Dos Stamps Added) :��8��5͌!5 ��u	
     (DateStamper Added) �8ͅ�#!4:=��!4:2��u	
 �S�u	 initializes the directory of an B/P BIOS RAM Disk.  The drive
 may be specified on the command line, or may be configured with ZCNFG
 in the range of "A"-"P".  Either P2Dos (CP/M+ compatible) or DateStamper
 or both types of File Date/Time stamping may be added at format time
 with option flags.
 WARNING - Any directory already on the RAM Drive will be destroyed!

Syntax:
         �S�u	 [d:][/][Q][D][P]
Examples:
         �S�u	         - Initialize Default RAM w/query
         �S�u	 P:      - Initialize Drive P:
         �S�u	 Q       - Initialize RAM without query
         �S�u	 N: /PD  - Init Drive N: w/both Stamps
         �S�u	 //      - display this message
Options:
  Q - No Confirmation Query
  P - Format for P2Dos (CP/M+ compatible) File Stamps
  D - Format for DateStamper(tm) File Stamps
 �{[�K�?�u	
+++ Drive   :���u	 :  �>*>'!>!>!>!!>$!>B* o�*	|�(}�(o0$~!
��:
�°�u	INIRAMD �~� �#�Q 2
�P 2�D 2��:O�2:O���]T 6���:����` 6!#]T 6 ���6 #�! ��kb 6 ��� !!�TIME&�AT                                   !               !               !               T               I               M               E               ��͵	����}	���~#�?���(�(�(�(���0����	���	���	���	��͵	���V#^#����	�����:d*i��	���[k���[m��
�*e:b�W~�(Ͳ
������*w�����[g�(�
:c��
�������(��
�%-���%(�\ ������D(L�2(C�3(;�.(+�+(*�>(�R(�I(�N ү�$,�}lg��0�g�|���e�| |d͘

͘
�0��/�0��Gz�0 �A(��x��~�(#�\ ~#���z�����O*�|�(�+ �~�(G���
� �������
���� �����"�|�(� "]�|�(D~�!8?� ~2_#~2`#~2a b ��̀��:_O	��̀� �������z!b6 #�s#r#�6 #s#r#�6 #�� �����s#r#�~#���\ �#�!�~������(# ������        ��͑(~�� (��#�����$ ���" *�|�(~#fo���"��|�� >Z�� ���(* >�O>��G>Z��  ������ ��|�� ;����#���! ~��#~��3ENV����Y �kX�k���������k�� �kO�G������� �ѷ��>��>
����͇��͍���>������ ß����&dͲ&
Ͳ������.��,0��g}� �|�> (��0G��|�����O>�������a��{��_���������o��* o����*� j��ə                                                                                                                      