�Z3ENV  * ͍"	͉��l 5�� Start/Stop Z8S180 Turbo Mode  V1.3   20 Jul 96
 �s�1�:] ��+( �-(� �ny2�*	 �~�[ (= ^#V�.Z~�� �!��~�B #~�/ #~�P(+��
+++ Not B/P Bios ... aborting +++ �! "�~2�#~2�#N#F�C��Ly��2�(;��
+++ This routine ONLY for Z180 and above CPU! +++ �!���!zͶ(B��
+++ Do Not recognize this B/P Bios Type +++ �-YS-18-FX-DX ��

...Checking CPU... ���  Measured :  :��3�� MHz :] � �:��(-��
+++ Can't Change...Not Z8S180/Z182! �*�##^#V�8�:�(M�(4��
+++ In Turbo Mode Already ... aborting +++ ��:�r+s+��G~�8w+�>J� 3��
+++ In SLOW Mode Already ... aborting +++ ��)�r+s+���8�?w+����~2�>?����:�2�!��(J*�:�w#:�w#�K�q#p>?���

+++ Failed...Entry Values Restored +++ ���

 -->> Computer set to  :��(
��HIGH ��LOW �� Speed!    ( :��3�� MHz) *�~*	.+w���
 ��� switches the speed bit for the Z8S180/182 into or out
 of the high-speed mode using B/P Bios Config features.
 It can optionally alter the Number of Memory Waits for
 slow memory chips.

Syntax:

	 ��� + [n]   - Switch to High-Speed [add "n" waits]
	 ��� - [n]   - Switch to Low-Speed [reduce "n" waits]
	 ���         - Show Current Speed
	 ��� //      - display this message �&�{��� >N�� >N�:��(�  K � ��� >N�:���(�{�*�#~�!����~#fo���0#�2��8O��9G�8��	> (/2�ɒ���t^M<0(`NB1'!RD9)"H:2#	���� #������x��~� ����:m �  :^ �08	�
0����ɯ����� ��z� �����Ʌo�$�:
��9��TURBO �* o�!1~�����R(# ������        ���(~�� (�j#�����$ �]�" *�|�(~#fo�����( *�|����*�|�(~���"��|�� >Z�� ����(* >�O>��G>Z��  ������� ��|�� ���������! ~��#~��3ENV������� ~#� ����	(�j�(��
(	�(� ����y/�<G> �j���>�j>
�j������&d�I&
�I�]����.��,0��g}� �|�> (��0G�j|�����O>�u������}����o��* o��                                                                                                                         