FasdUAS 1.101.10   ��   ��    k             l      ��  ��    $  version 2.1.0 - Jan 25, 2010      � 	 	 <   v e r s i o n   2 . 1 . 0   -   J a n   2 5 ,   2 0 1 0     
  
 l      ��  ��    , & sends contents of clipboard to Stata      �   L   s e n d s   c o n t e n t s   o f   c l i p b o a r d   t o   S t a t a        l      ��  ��    U O allows running from command window, as a temporary do-file or via a menu item      �   �   a l l o w s   r u n n i n g   f r o m   c o m m a n d   w i n d o w ,   a s   a   t e m p o r a r y   d o - f i l e   o r   v i a   a   m e n u   i t e m        l      ��  ��    J D args are: { "command" | "menu" | "dofile" } [ name-of-tmp-dofile ]      �   �   a r g s   a r e :   {   " c o m m a n d "   |   " m e n u "   |   " d o f i l e "   }   [   n a m e - o f - t m p - d o f i l e   ]        i         I     �� ��
�� .aevtoappnull  �   � ****  o      ���� 0 args  ��    k    t        l     �� ! "��   !  - initializations    " � # # " -   i n i t i a l i z a t i o n s    $ % $ q       & & �� '�� 0 numargs numArgs ' �� (�� 0 pasteme pasteMe ( �� )�� 0 dothis doThis ) ������ 0 	tmpdofile 	tmpDoFile��   %  * + * q       , , ������ 0 howmanystatas howManyStatas��   +  - . - q       / / ������ $0 defaulttmpdofile defaultTmpDoFile��   .  0 1 0 r      2 3 2 m      4 4 � 5 5  f e e d S t a t a . d o 3 o      ���� $0 defaulttmpdofile defaultTmpDoFile 1  6 7 6 q     8 8 ������ 0 uiok UIOK��   7  9 : 9 l   �� ; <��   ; . ( first check that UI scripting will work    < � = = P   f i r s t   c h e c k   t h a t   U I   s c r i p t i n g   w i l l   w o r k :  > ? > O     @ A @ Z     B C�� D B 1    ��
�� 
uien C r     E F E m    ��
�� boovtrue F o      ���� 0 uiok UIOK��   D r     G H G m    ��
�� boovfals H o      ���� 0 uiok UIOK A m     I I�                                                                                  sevs   alis    |  Malcom                     �ꫣH+   .�System Events.app                                               <����        ����  	                CoreServices    ����      ��e     .�  ��  ��  4Malcom:System:Library:CoreServices:System Events.app  $  S y s t e m   E v e n t s . a p p    M a l c o m  -System/Library/CoreServices/System Events.app   / ��   ?  J�� J l  t K L M K Z   t N O�� P N H     Q Q o    ���� 0 uiok UIOK O O    < R S R k   # ; T T  U V U I  # ,�� W X
�� .sysodlogaskr        TEXT W m   # $ Y Y � Z Z � W h e n   S y s t e m   P r e f r e n c e s   o p e n s ,   b e   s u r e   t h a t   ' E n a b l e   a c c e s s   f o r   a s s i s t i v e   d e v i c e s '   i s   c h e c k e d ,   t h e n   t r y   a g a i n . X �� [��
�� 
btns [ J   % ( \ \  ]�� ] m   % & ^ ^ � _ _  O K��  ��   V  ` a ` I  - 2������
�� .miscactvnull��� ��� null��  ��   a  b�� b r   3 ; c d c 4   3 7�� e
�� 
xppb e m   5 6 f f � g g H c o m . a p p l e . p r e f e r e n c e . u n i v e r s a l a c c e s s d 1   7 :��
�� 
xpcp��   S m      h h�                                                                                  sprf   alis    j  Malcom                     �ꫣH+   	System Preferences.app                                          좿�5�        ����  	                Applications    ����      ���     	  *Malcom:Applications:System Preferences.app  .  S y s t e m   P r e f e r e n c e s . a p p    M a l c o m  #Applications/System Preferences.app   / ��  ��   P k   ?t i i  j k j l  ? ?�� l m��   l ' ! check proper number of arguments    m � n n B   c h e c k   p r o p e r   n u m b e r   o f   a r g u m e n t s k  o p o r   ? D q r q l  ? B s���� s n   ? B t u t 1   @ B��
�� 
leng u o   ? @���� 0 args  ��  ��   r o      ���� 0 numargs numArgs p  v w v Q   E � x y z x k   H � { {  | } | r   H N ~  ~ n   H L � � � 4   I L�� �
�� 
cobj � m   J K����  � o   H I���� 0 args    o      ���� 0 dothis doThis }  � � � Z   O e � ����� � H   O Y � � E   O X � � � J   O V � �  � � � m   O P � � � � �  c o m m a n d �  � � � m   P Q � � � � �  m e n u �  ��� � m   Q T � � � � �  d o f i l e��   � o   V W���� 0 dothis doThis � n  \ a � � � I   ] a�������� 0 badfirstarg badFirstArg��  ��   �  f   \ ]��  ��   �  ��� � Z   f � � ��� � � ?   f i � � � o   f g���� 0 numargs numArgs � m   g h����  � k   l � � �  � � � r   l r � � � n   l p � � � 4   m p�� �
�� 
cobj � m   n o����  � o   l m���� 0 args   � o      ���� 0 	tmpdofile 	tmpDoFile �  ��� � Z   s � � ����� � =   s x � � � o   s t���� 0 	tmpdofile 	tmpDoFile � m   t w � � � � �   � r   { ~ � � � o   { |���� $0 defaulttmpdofile defaultTmpDoFile � o      ���� 0 	tmpdofile 	tmpDoFile��  ��  ��  ��   � r   � � � � � o   � ����� $0 defaulttmpdofile defaultTmpDoFile � o      ���� 0 	tmpdofile 	tmpDoFile��   y R      ������
�� .ascrerr ****      � ****��  ��   z l  � � � � � � n  � � � � � I   � ��������� 0 badfirstarg badFirstArg��  ��   �  f   � � �   no arguments    � � � �    n o   a r g u m e n t s w  � � � l  � ���������  ��  ��   �  � � � l  � ��� � ���   � U O grab clipboard, strip totally blank lines, to check if there is anything to do    � � � � �   g r a b   c l i p b o a r d ,   s t r i p   t o t a l l y   b l a n k   l i n e s ,   t o   c h e c k   i f   t h e r e   i s   a n y t h i n g   t o   d o �  � � � l  � ��� � ���   � 9 3   Aside: perhaps this should be on the emacs side?    � � � � f       A s i d e :   p e r h a p s   t h i s   s h o u l d   b e   o n   t h e   e m a c s   s i d e ? �  � � � l  � ��� � ���   � X R   for now it will stay here... could be wrong behavior, plus it is simpler to do     � � � � �       f o r   n o w   i t   w i l l   s t a y   h e r e . . .   c o u l d   b e   w r o n g   b e h a v i o r ,   p l u s   i t   i s   s i m p l e r   t o   d o   �  � � � l  � ��� � ���   �       in Applescript (!)    � � � � .           i n   A p p l e s c r i p t   ( ! ) �  � � � r   � � � � � n  � � � � � I   � ��� ����� "0 stripblanklines stripBlankLines �  ��� � I  � �������
�� .JonsgClp****    ��� null��  ��  ��  ��   �  f   � � � o      ���� 0 pasteme pasteMe �  � � � Z   � � � ����� � =   � � � � � o   � ����� 0 pasteme pasteMe � m   � � � � � � �   � O   � � � � � I  � ��� � �
�� .sysodlogaskr        TEXT � m   � � � � � � � , N o t h i n g   t o   s e n d   S t a t a ! � �� ���
�� 
btns � J   � � � �  ��� � m   � � � � � � �  C a n c e l��  ��   � m   � � � ��                                                                                  MACS   alis    `  Malcom                     �ꫣH+   .�
Finder.app                                                      .�����        ����  	                CoreServices    ����      ���<     .�  ��  ��  -Malcom:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p    M a l c o m  &System/Library/CoreServices/Finder.app  / ��  ��  ��   �  � � � l  � ���������  ��  ��   �  � � � l  � ��� � ���   � \ V in the best of worlds, it would be possible to allow looping through the instances of    � � � � �   i n   t h e   b e s t   o f   w o r l d s ,   i t   w o u l d   b e   p o s s i b l e   t o   a l l o w   l o o p i n g   t h r o u g h   t h e   i n s t a n c e s   o f �  � � � l  � ��� � ���   � 5 /   Stata to send the same code to each instance    � � � � ^       S t a t a   t o   s e n d   t h e   s a m e   c o d e   t o   e a c h   i n s t a n c e �  � � � O   � � � � � r   � � � � � l  � � ����� � 6  � � � � � 2   � ���
�� 
prcs � E   � � � � � 1   � ���
�� 
pnam � m   � �   � 
 S t a t a��  ��   � o      ���� 0 	thestatas 	theStatas � m   � ��                                                                                  sevs   alis    |  Malcom                     �ꫣH+   .�System Events.app                                               <����        ����  	                CoreServices    ����      ��e     .�  ��  ��  4Malcom:System:Library:CoreServices:System Events.app  $  S y s t e m   E v e n t s . a p p    M a l c o m  -System/Library/CoreServices/System Events.app   / ��   �  l  � ���������  ��  ��    r   � � l  � �	����	 I  � ���
��
�� .corecnte****       ****
 o   � ����� 0 	thestatas 	theStatas��  ��  ��   o      ���� 0 howmanystatas howManyStatas  Z   �C�� =  � � o   � ����� 0 howmanystatas howManyStatas m   � �����   O   � k   �  I  � �������
�� .sysobeepnull��� ��� long��  ��   �� I  ���
�� .sysodlogaskr        TEXT m   � � " N o   S t a t a   r u n n i n g ! ��~
� 
btns J   �} m   �    C a n c e l�}  �~  ��   m   � �!!�                                                                                  MACS   alis    `  Malcom                     �ꫣH+   .�
Finder.app                                                      .�����        ����  	                CoreServices    ����      ���<     .�  ��  ��  -Malcom:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p    M a l c o m  &System/Library/CoreServices/Finder.app  / ��  ��   k  C"" #$# Z  /%&�|�{% ?  '(' o  �z�z 0 howmanystatas howManyStatas( m  �y�y & k  +)) *+* O  ),-, I (�x./
�x .sysodlogaskr        TEXT. m  00 �11 @ n o t h i n g   f o r   m u l t i p l e   s t a t a ' s   y e t/ �w2�v
�w 
btns2 J  $33 4�u4 m  "55 �66  C a n c e l�u  �v  - m  77�                                                                                  MACS   alis    `  Malcom                     �ꫣH+   .�
Finder.app                                                      .�����        ����  	                CoreServices    ����      ���<     .�  ��  ��  -Malcom:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p    M a l c o m  &System/Library/CoreServices/Finder.app  / ��  + 898 l **�t:;�t  : : 4 Stata can use the same name for different processes   ; �<< h   S t a t a   c a n   u s e   t h e   s a m e   n a m e   f o r   d i f f e r e n t   p r o c e s s e s9 =>= l **�s?@�s  ? J D so... the it is impossible to cycle through Stata processes by name   @ �AA �   s o . . .   t h e   i t   i s   i m p o s s i b l e   t o   c y c l e   t h r o u g h   S t a t a   p r o c e s s e s   b y   n a m e> B�rB l  **�qCD�q  C � � tell application "System Events"					set theStatas to (the file of every process whose name contains "Stata")				 end tell				repeat with aStata in theStatas				end repeat
				   D �EEj   t e l l   a p p l i c a t i o n   " S y s t e m   E v e n t s "  	 	 	 	 	 s e t   t h e S t a t a s   t o   ( t h e   f i l e   o f   e v e r y   p r o c e s s   w h o s e   n a m e   c o n t a i n s   " S t a t a " )  	 	 	 	   e n d   t e l l  	 	 	 	 r e p e a t   w i t h   a S t a t a   i n   t h e S t a t a s  	 	 	 	 e n d   r e p e a t 
 	 	 	 	�r  �|  �{  $ FGF l 00�pHI�p  H : 4 know there is exactly one instance of Stata running   I �JJ h   k n o w   t h e r e   i s   e x a c t l y   o n e   i n s t a n c e   o f   S t a t a   r u n n i n gG KLK l 00�oMN�o  M   can finally get to work   N �OO 0   c a n   f i n a l l y   g e t   t o   w o r kL P�nP O  0CQRQ r  4BSTS l 4>U�m�lU l 4>V�k�jV n  4>WXW 1  :>�i
�i 
pnamX l 4:Y�h�gY n  4:Z[Z 4 7:�f\
�f 
cobj\ m  89�e�e [ o  47�d�d 0 	thestatas 	theStatas�h  �g  �k  �j  �m  �l  T o      �c�c 0 thestataname theStataNameR m  01]]�                                                                                  sevs   alis    |  Malcom                     �ꫣH+   .�System Events.app                                               <����        ����  	                CoreServices    ����      ��e     .�  ��  ��  4Malcom:System:Library:CoreServices:System Events.app  $  S y s t e m   E v e n t s . a p p    M a l c o m  -System/Library/CoreServices/System Events.app   / ��  �n   ^_^ l DD�b�a�`�b  �a  �`  _ `a` l DD�_bc�_  b $  Stata *must* be made active		   c �dd <   S t a t a   * m u s t *   b e   m a d e   a c t i v e 	 	a efe O  DUghg I OT�^�]�\
�^ .miscactvnull��� ��� null�]  �\  h 4  DL�[i
�[ 
cappi o  HK�Z�Z 0 thestataname theStataNamef jkj l VV�Y�X�W�Y  �X  �W  k l�Vl Z  Vtmn�Uom = V[pqp o  VW�T�T 0 dothis doThisq m  WZrr �ss  c o m m a n dn n ^gtut I  _g�Sv�R�S 0 pastetmpstata pasteTmpStatav wxw o  _b�Q�Q 0 thestataname theStataNamex y�Py o  bc�O�O 0 pasteme pasteMe�P  �R  u  f  ^_�U  o n jtz{z I  kt�N|�M�N 0 dotmpdofile doTmpDofile| }~} o  kn�L�L 0 thestataname theStataName~ � o  no�K�K 0 	tmpdofile 	tmpDoFile� ��J� o  op�I�I 0 dothis doThis�J  �M  {  f  jk�V   L ' !- from test of UI being turned on    M ��� B -   f r o m   t e s t   o f   U I   b e i n g   t u r n e d   o n��    ��� l     �H�G�F�H  �G  �F  � ��� i    ��� I      �E�D�C�E 0 badfirstarg badFirstArg�D  �C  � O     ��� I   �B��
�B .sysodlogaskr        TEXT� m    �� ��� \ T h e   f i r s t   a r g u m e n t   m u s t   b e   " c o m m a n d "   o r   " m e n u "� �A��@
�A 
btns� J    	�� ��?� m    �� ���  C a n c e l�?  �@  � m     ���                                                                                  MACS   alis    `  Malcom                     �ꫣH+   .�
Finder.app                                                      .�����        ����  	                CoreServices    ����      ���<     .�  ��  ��  -Malcom:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p    M a l c o m  &System/Library/CoreServices/Finder.app  / ��  � ��� l     �>�=�<�>  �=  �<  � ��� i    ��� I      �;��:�; 0 dotmpdofile doTmpDofile� ��� o      �9�9 0 	stataname 	stataName� ��� o      �8�8 0 	tmpdofile 	tmpDoFile� ��7� o      �6�6 0 dowhat doWhat�7  �:  � k     ��� ��� l     �5���5  � K E if multiple instances ever work, be sure this gets written just once   � ��� �   i f   m u l t i p l e   i n s t a n c e s   e v e r   w o r k ,   b e   s u r e   t h i s   g e t s   w r i t t e n   j u s t   o n c e� ��� q      �� �4��4 
0 tmpdir  � �3��3 0 	tmpdofile 	tmpDoFile� �2�1�2 "0 stupidapplefile stupidAppleFile�1  � ��� r     ��� I    �0��/
�0 .sysoexecTEXT���     TEXT� m     �� ��� 8 g e t c o n f   D A R W I N _ U S E R _ T E M P _ D I R�/  � o      �.�. 
0 tmpdir  � ��� r    ��� l   ��-�,� b    ��� o    	�+�+ 
0 tmpdir  � o   	 
�*�* 0 	tmpdofile 	tmpDoFile�-  �,  � o      �)�) 0 	tmpdofile 	tmpDoFile� ��� l   �(���(  � < 6 need applescript-style file name to write to the file   � ��� l   n e e d   a p p l e s c r i p t - s t y l e   f i l e   n a m e   t o   w r i t e   t o   t h e   f i l e� ��� r    ��� 4    �'�
�' 
psxf� o    �&�& 0 	tmpdofile 	tmpDoFile� o      �%�% "0 stupidapplefile stupidAppleFile� ��� Q    a���� k    A�� ��� I   �$��
�$ .rdwropenshor       file� o    �#�# "0 stupidapplefile stupidAppleFile� �"��!
�" 
perm� m    � 
�  boovtrue�!  � ��� I    '���
� .rdwrseofnull���     ****� o     !�� "0 stupidapplefile stupidAppleFile� ���
� 
set2� m   " #��  �  � ��� I  ( 3���
� .rdwrwritnull���     ****� l  ( -���� I  ( -���
� .JonsgClp****    ��� null�  �  �  �  � ���
� 
refn� o   . /�� "0 stupidapplefile stupidAppleFile�  � ��� I  4 ;���
� .rdwrwritnull���     ****� o   4 5�
� 
ret � ���
� 
refn� o   6 7�� "0 stupidapplefile stupidAppleFile�  � ��� I  < A���

� .rdwrclosnull���     ****� o   < =�	�	 "0 stupidapplefile stupidAppleFile�
  �  � R      ���
� .ascrerr ****      � ****�  �  � k   I a�� ��� I  I N���
� .rdwrclosnull���     ****� o   I J�� "0 stupidapplefile stupidAppleFile�  � ��� O   O a��� I  S `���
� .sysodlogaskr        TEXT� m   S T�� ��� L H a d   t r o u b l e   w i t h   t h e   t e m p o r a r y   d o - f i l e� � ���
�  
btns� J   W \�� ���� m   W Z�� ���  C a n c e l��  ��  � m   O P���                                                                                  MACS   alis    `  Malcom                     �ꫣH+   .�
Finder.app                                                      .�����        ����  	                CoreServices    ����      ���<     .�  ��  ��  -Malcom:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p    M a l c o m  &System/Library/CoreServices/Finder.app  / ��  �  � ��� l  b b������  � #  applescript really is a pita   � ��� :   a p p l e s c r i p t   r e a l l y   i s   a   p i t a� ��� O   b q��� I  k p������
�� .miscactvnull��� ��� null��  ��  � 4   b h���
�� 
capp� o   f g���� 0 	stataname 	stataName� ���� Z   r ������� =   r w   o   r s���� 0 dowhat doWhat m   s v �  m e n u� Q   z � I   } ������� 0 	doviamenu 	doViaMenu �� o   ~ ���� 0 	stataname 	stataName��  ��   R      ������
�� .ascrerr ****      � ****��  ��   k   � �		 

 I   � ������� "0 createmenuitems createMenuItems  o   � ����� 0 	stataname 	stataName �� o   � ����� 0 	tmpdofile 	tmpDoFile��  ��    l  � �����   8 2 need to be sure the menu item exists on first try    � d   n e e d   t o   b e   s u r e   t h e   m e n u   i t e m   e x i s t s   o n   f i r s t   t r y  I  � �����
�� .sysodelanull��� ��� nmbr m   � ����� ��   �� Q   � � I   � ������� 0 	doviamenu 	doViaMenu �� o   � ����� 0 	stataname 	stataName��  ��   R      ������
�� .ascrerr ****      � ****��  ��   O   � � I  � ��� !
�� .sysodlogaskr        TEXT  m   � �"" �## v S o m e t h i n g   w e n t   w r o n g . . .   i s   y o u r   s t a t a   v e r s i o n   s e t   p r o p e r l y ?! ��$��
�� 
btns$ J   � �%% &��& m   � �'' �((  C a n c e l��  ��   m   � �))�                                                                                  MACS   alis    `  Malcom                     �ꫣH+   .�
Finder.app                                                      .�����        ����  	                CoreServices    ����      ���<     .�  ��  ��  -Malcom:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p    M a l c o m  &System/Library/CoreServices/Finder.app  / ��  ��  ��  � l  � �*+,* Q   � �-./- O   � �010 I  � ���2��
�� .aevtodocnull  �    alis2 o   � ����� "0 stupidapplefile stupidAppleFile��  1 4   � ���3
�� 
capp3 o   � ����� 0 	stataname 	stataName. R      ������
�� .ascrerr ****      � ****��  ��  / O   � �454 I  � ���67
�� .sysodlogaskr        TEXT6 m   � �88 �99 R H a d   t r o u b l e   r u n n i n g   v i a   t e m p o r a r y   d o - f i l e7 ��:��
�� 
btns: J   � �;; <��< m   � �== �>>  C a n c e l��  ��  5 m   � �??�                                                                                  MACS   alis    `  Malcom                     �ꫣH+   .�
Finder.app                                                      .�����        ����  	                CoreServices    ����      ���<     .�  ��  ��  -Malcom:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p    M a l c o m  &System/Library/CoreServices/Finder.app  / ��  + < 6 doing via dofile; needs fixing for multiple instances   , �@@ l   d o i n g   v i a   d o f i l e ;   n e e d s   f i x i n g   f o r   m u l t i p l e   i n s t a n c e s��  � ABA l     ��������  ��  ��  B CDC i    EFE I      ��G���� 0 	doviamenu 	doViaMenuG H��H o      ���� 0 	stataname 	stataName��  ��  F O     /IJI Z    .KL����K 1    ��
�� 
uienL O    *MNM I   )��O��
�� .prcsclicuiel    ��� uielO n    %PQP 4   " %��R
�� 
menIR m   # $SS �TT  r u n   t m p   f i l eQ n    "UVU 4    "��W
�� 
menEW m     !���� V n    XYX 4    ��Z
�� 
menIZ m    [[ �\\ & A p p l e s c r i p t   h e l p e r sY n    ]^] 4    ��_
�� 
menE_ m    ���� ^ n    `a` 4    ��b
�� 
mbrib m    cc �dd  U s e ra 4    ��e
�� 
mbare m    ���� ��  N 4    ��f
�� 
pcapf o    ���� 0 	stataname 	stataName��  ��  J m     gg�                                                                                  sevs   alis    |  Malcom                     �ꫣH+   .�System Events.app                                               <����        ����  	                CoreServices    ����      ��e     .�  ��  ��  4Malcom:System:Library:CoreServices:System Events.app  $  S y s t e m   E v e n t s . a p p    M a l c o m  -System/Library/CoreServices/System Events.app   / ��  D hih l     ��������  ��  ��  i jkj i    lml I      ��n���� "0 createmenuitems createMenuItemsn opo o      ���� 0 	stataname 	stataNamep q��q o      ���� 0 	tmpdofile 	tmpDoFile��  ��  m O     ;rsr Z    :tu����t 1    ��
�� 
uienu O    6vwv k    5xx yzy l   ��{|��  { * $ get the command window to the front   | �}} H   g e t   t h e   c o m m a n d   w i n d o w   t o   t h e   f r o n tz ~~ I   ����
�� .prcskprsnull���    utxt� m    �� ���  4� �����
�� 
faal� m    ��
�� eMdsKcmd��   ��� I   !�����
�� .prcskprsnull���    utxt� b    ��� m    �� ��� r w i n d o w   m e n u   a p p e n d   s u b m e n u   " s t U s e r "   " A p p l e s c r i p t   h e l p e r s "� o    ��
�� 
ret ��  � ��� I  " -�����
�� .prcskprsnull���    utxt� b   " )��� b   " '��� b   " %��� m   " #�� ��� � w i n d o w   m e n u   a p p e n d   i t e m   " A p p l e s c r i p t   h e l p e r s "   " r u n   t m p   f i l e "   " d o  � o   # $���� 0 	tmpdofile 	tmpDoFile� m   % &�� ���  "� o   ' (��
�� 
ret ��  � ���� I  . 5�����
�� .prcskprsnull���    utxt� b   . 1��� m   . /�� ��� & w i n d o w   m e n u   r e f r e s h� o   / 0��
�� 
ret ��  ��  w 4    ���
�� 
pcap� o    ���� 0 	stataname 	stataName��  ��  s m     ���                                                                                  sevs   alis    |  Malcom                     �ꫣH+   .�System Events.app                                               <����        ����  	                CoreServices    ����      ��e     .�  ��  ��  4Malcom:System:Library:CoreServices:System Events.app  $  S y s t e m   E v e n t s . a p p    M a l c o m  -System/Library/CoreServices/System Events.app   / ��  k ��� l     ��������  ��  ��  � ��� i    ��� I      ������� 0 pastetmpstata pasteTmpStata� ��� o      ���� 0 	stataname 	stataName� ���� o      ���� 0 pasteme pasteMe��  ��  � k     ��� ��� q      �� ������ 0 oldclipboard oldClipBoard��  � ��� r     ��� I    ������
�� .JonsgClp****    ��� null��  ��  � o      ���� 0 oldclipboard oldClipBoard� ��� Q    z���� k    V�� ��� I   �����
�� .JonspClpnull���     ****� l   ������ c    ��� o    ���� 0 pasteme pasteMe� m    ��
�� 
ctxt��  ��  ��  � ���� O    V��� Z    U������� 1    ��
�� 
uien� O    Q��� k   % P�� ��� I  % ,����
�� .prcskprsnull���    utxt� m   % &�� ���  4� ���~
� 
faal� m   ' (�}
�} eMdsKcmd�~  � ��� l  - -�|���|  �  delay 1   � ���  d e l a y   1� ��� I  - @�{��z
�{ .prcsclicuiel    ��� uiel� n  - <��� 4   7 <�y�
�y 
menI� l 	 8 ;��x�w� m   8 ;�� ��� 
 P a s t e�x  �w  � n   - 7��� 4   4 7�v�
�v 
menE� m   5 6�� ���  E d i t� n  - 4��� 4   1 4�u�
�u 
mbri� m   2 3�� ���  E d i t� 4   - 1�t�
�t 
mbar� m   / 0�s�s �z  � ��� l  A A�r���r  � > 8 added delay when seeing odd behavior on machine at work   � ��� p   a d d e d   d e l a y   w h e n   s e e i n g   o d d   b e h a v i o r   o n   m a c h i n e   a t   w o r k� ��� l  A A�q���q  � U O it seems that a delay of under 0.2 seconds makes things weird on fast machines   � ��� �   i t   s e e m s   t h a t   a   d e l a y   o f   u n d e r   0 . 2   s e c o n d s   m a k e s   t h i n g s   w e i r d   o n   f a s t   m a c h i n e s� ��� I  A H�p��o
�p .sysodelanull��� ��� nmbr� m   A D�� ?ə������o  � ��n� I  I P�m��l
�m .prcskprsnull���    utxt� o   I L�k
�k 
ret �l  �n  � 4    "�j�
�j 
pcap� o     !�i�i 0 	stataname 	stataName��  ��  � m    ���                                                                                  sevs   alis    |  Malcom                     �ꫣH+   .�System Events.app                                               <����        ����  	                CoreServices    ����      ��e     .�  ��  ��  4Malcom:System:Library:CoreServices:System Events.app  $  S y s t e m   E v e n t s . a p p    M a l c o m  -System/Library/CoreServices/System Events.app   / ��  ��  � R      �h�g�f
�h .ascrerr ****      � ****�g  �f  � k   ^ z�� ��� I  ^ c�e��d
�e .JonspClpnull���     ****� o   ^ _�c�c 0 oldclipboard oldClipBoard�d  � ��b� O   d z��� I  j y�a 
�a .sysodlogaskr        TEXT  m   j m � V H a d   t r o u b l e   p a s t i n g   t o   S t a t a   c o m m a n d   w i n d o w �`�_
�` 
btns J   p u �^ m   p s �  C a n c e l�^  �_  � m   d g		�                                                                                  MACS   alis    `  Malcom                     �ꫣH+   .�
Finder.app                                                      .�����        ����  	                CoreServices    ����      ���<     .�  ��  ��  -Malcom:System:Library:CoreServices:Finder.app    
 F i n d e r . a p p    M a l c o m  &System/Library/CoreServices/Finder.app  / ��  �b  � 
�]
 I  { ��\�[
�\ .JonspClpnull���     **** o   { |�Z�Z 0 oldclipboard oldClipBoard�[  �]  �  l     �Y�X�W�Y  �X  �W    i     I      �V�U�V "0 stripblanklines stripBlankLines �T o      �S�S 0 thetext theText�T  �U   k     i  q       �R�R 0 theparas theParas �Q�P�Q 0 thepara thePara�P    r      l    �O�N n      2   �M
�M 
cpar o     �L�L 0 thetext theText�O  �N   o      �K�K 0 theparas theParas  !  r    
"#" J    �J�J  # o      �I�I 0 thecleanstuff theCleanStuff! $%$ X    ^&�H'& k    Y(( )*) r    +,+ m    �G
�G boovtrue, o      �F�F 0 eraseme  * -.- X    I/�E0/ Z   1 D12�D�C1 H   1 833 E  1 7454 J   1 566 787 m   1 299 �::   8 ;�B; m   2 3<< �==  	�B  5 o   5 6�A�A 0 achar aChar2 k   ; @>> ?@? r   ; >ABA m   ; <�@
�@ boovfalsB o      �?�? 0 eraseme  @ C�>C  S   ? @�>  �D  �C  �E 0 achar aChar0 n   " %DED 2   # %�=
�= 
cha E o   " #�<�< 0 thepara thePara. F�;F Z   J YGH�:�9G H   J LII o   J K�8�8 0 eraseme  H r   O UJKJ l  O RL�7�6L c   O RMNM o   O P�5�5 0 thepara theParaN m   P Q�4
�4 
TEXT�7  �6  K l     O�3�2O n      PQP  ;   S TQ o   R S�1�1 0 thecleanstuff theCleanStuff�3  �2  �:  �9  �;  �H 0 thepara thePara' o    �0�0 0 theparas theParas% RSR r   _ dTUT o   _ `�/
�/ 
ret U l     V�.�-V 1   ` c�,
�, 
txdl�.  �-  S W�+W L   e iXX l  e hY�*�)Y c   e hZ[Z o   e f�(�( 0 thecleanstuff theCleanStuff[ m   f g�'
�' 
TEXT�*  �)  �+   \]\ l     �&�%�$�&  �%  �$  ] ^�#^ l     �"�!� �"  �!  �   �#       �_`abcdefgh������  _ ��������������
� .aevtoappnull  �   � ****� 0 badfirstarg badFirstArg� 0 dotmpdofile doTmpDofile� 0 	doviamenu 	doViaMenu� "0 createmenuitems createMenuItems� 0 pastetmpstata pasteTmpStata� "0 stripblanklines stripBlankLines� 0 	thestatas 	theStatas� 0 thestataname theStataName�  �  �  �  �  ` � �
�	ij�
� .aevtoappnull  �   � ****�
 0 args  �	  i �������� � 0 args  � 0 numargs numArgs� 0 pasteme pasteMe� 0 dothis doThis� 0 	tmpdofile 	tmpDoFile� 0 howmanystatas howManyStatas� $0 defaulttmpdofile defaultTmpDoFile�  0 uiok UIOKj + 4 I�� h Y�� ^������ f������ � � ��� ��������� � � � ���k�� ������05����r����
�� 
uien
�� 
btns
�� .sysodlogaskr        TEXT
�� .miscactvnull��� ��� null
�� 
xppb
�� 
xpcp
�� 
leng
�� 
cobj�� 0 badfirstarg badFirstArg��  ��  
�� .JonsgClp****    ��� null�� "0 stripblanklines stripBlankLines
�� 
prcsk  
�� 
pnam�� 0 	thestatas 	theStatas
�� .corecnte****       ****
�� .sysobeepnull��� ��� long�� 0 thestataname theStataName
�� 
capp�� 0 pastetmpstata pasteTmpStata�� 0 dotmpdofile doTmpDofile�u�E�O� *�,E eE�Y fE�UO� "� ���kvl O*j O*��/*�,FUY7��,E�O E��k/E�O��a mv� 
)j+ Y hO�k ��l/E�O�a   �E�Y hY �E�W X  )j+ O)*j k+ E�O�a   a  a �a kvl UY hO� *a -a [a ,\Za @1E` UO_ j  E�O�j  a  *j !Oa "�a #kvl UY 6�k a  a $�a %kvl UOPY hO� _ �k/a ,E` &UO*a '_ &/ *j UO�a (  )_ &�l+ )Y )_ &��m+ *a �������lm���� 0 badfirstarg badFirstArg��  ��  l  m �������
�� 
btns
�� .sysodlogaskr        TEXT�� � ���kvl Ub �������no���� 0 dotmpdofile doTmpDofile�� ��p�� p  �������� 0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFile�� 0 dowhat doWhat��  n ������������ 0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFile�� 0 dowhat doWhat�� 
0 tmpdir  �� "0 stupidapplefile stupidAppleFileo ��������������������������������������������"'��8=
�� .sysoexecTEXT���     TEXT
�� 
psxf
�� 
perm
�� .rdwropenshor       file
�� 
set2
�� .rdwrseofnull���     ****
�� .JonsgClp****    ��� null
�� 
refn
�� .rdwrwritnull���     ****
�� 
ret 
�� .rdwrclosnull���     ****��  ��  
�� 
btns
�� .sysodlogaskr        TEXT
�� 
capp
�� .miscactvnull��� ��� null�� 0 	doviamenu 	doViaMenu�� "0 createmenuitems createMenuItems
�� .sysodelanull��� ��� nmbr
�� .aevtodocnull  �    alis�� ��j E�O��%E�O*�/E�O .��el O��jl O*j �l 	O��l 	O�j W X  �j O� �a a kvl UO*a �/ *j UO�a   I *�k+ W :X  *��l+ Okj O *�k+ W X  � a a a kvl UY 0 *a �/ �j UW X  � a a a kvl Uc ��F����qr���� 0 	doviamenu 	doViaMenu�� ��s�� s  ���� 0 	stataname 	stataName��  q ���� 0 	stataname 	stataNamer g��������c����[S��
�� 
uien
�� 
pcap
�� 
mbar
�� 
mbri
�� 
menE
�� 
menI
�� .prcsclicuiel    ��� uiel�� 0� ,*�,E $*�/ *�k/��/�k/��/�k/��/j 
UY hUd ��m����tu���� "0 createmenuitems createMenuItems�� ��v�� v  ������ 0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFile��  t ������ 0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFileu ������������������
�� 
uien
�� 
pcap
�� 
faal
�� eMdsKcmd
�� .prcskprsnull���    utxt
�� 
ret �� <� 8*�,E 0*�/ %���l O��%j O�%�%�%j O��%j UY hUe �������wx���� 0 pastetmpstata pasteTmpStata�� ��y�� y  ������ 0 	stataname 	stataName�� 0 pasteme pasteMe��  w �������� 0 	stataname 	stataName�� 0 pasteme pasteMe�� 0 oldclipboard oldClipBoardx ����������������������������������������	����
�� .JonsgClp****    ��� null
�� 
ctxt
�� .JonspClpnull���     ****
�� 
uien
�� 
pcap
�� 
faal
�� eMdsKcmd
�� .prcskprsnull���    utxt
�� 
mbar
�� 
mbri
�� 
menE
�� 
menI
�� .prcsclicuiel    ��� uiel
�� .sysodelanull��� ��� nmbr
�� 
ret ��  ��  
�� 
btns
�� .sysodlogaskr        TEXT�� �*j  E�O P��&j O� @*�,E 8*�/ -���l 	O*�k/��/��/�a /j Oa j O_ j 	UY hUW #X  �j Oa  a a a kvl UO�j f ������z{���� "0 stripblanklines stripBlankLines�� ��|�� |  ���� 0 thetext theText��  z ������~�}�|�� 0 thetext theText�� 0 theparas theParas� 0 thepara thePara�~ 0 thecleanstuff theCleanStuff�} 0 eraseme  �| 0 achar aChar{ 
�{�z�y�x�w9<�v�u�t
�{ 
cpar
�z 
kocl
�y 
cobj
�x .corecnte****       ****
�w 
cha 
�v 
TEXT
�u 
ret 
�t 
txdl�� j��-E�OjvE�O R�[��l kh eE�O )��-[��l kh ��lv� 
fE�OY h[OY��O� ��&�6FY h[OY��O�*�,FO��&g �s}�s }  ~~   I�r�
�r 
pcap� ���  S t a t a M Ph ���  S t a t a M P�  �  �  �  �   ascr  ��ޭ