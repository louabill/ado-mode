FasdUAS 1.101.10   ��   ��    k             l      ��  ��    ( " version 2.1.3 -  August 29, 2016      � 	 	 D   v e r s i o n   2 . 1 . 3   -     A u g u s t   2 9 ,   2 0 1 6     
  
 l      ��  ��    , & sends contents of clipboard to Stata      �   L   s e n d s   c o n t e n t s   o f   c l i p b o a r d   t o   S t a t a        l      ��  ��    U O allows running from command window, as a temporary do-file or via a menu item      �   �   a l l o w s   r u n n i n g   f r o m   c o m m a n d   w i n d o w ,   a s   a   t e m p o r a r y   d o - f i l e   o r   v i a   a   m e n u   i t e m        l      ��  ��    N H applescript bug: single bar in front of "include" causes compile error      �   �   a p p l e s c r i p t   b u g :   s i n g l e   b a r   i n   f r o n t   o f   " i n c l u d e "   c a u s e s   c o m p i l e   e r r o r        l      ��  ��    W Q args are: { "command" | "menu" | "dofile" || "include" } [ name-of-tmp-dofile ]      �   �   a r g s   a r e :   {   " c o m m a n d "   |   " m e n u "   |   " d o f i l e "   | |   " i n c l u d e "   }   [   n a m e - o f - t m p - d o f i l e   ]        i        !   I     �� "��
�� .aevtoappnull  �   � **** " o      ���� 0 args  ��   ! k    i # #  $ % $ l     �� & '��   &  - initializations    ' � ( ( " -   i n i t i a l i z a t i o n s %  ) * ) l     �� + ,��   + � �- NOTE: all vars using in the run handler MUST be local to keep this file from resaving itself ad-nauseum, messing up the git repo    , � - - -   N O T E :   a l l   v a r s   u s i n g   i n   t h e   r u n   h a n d l e r   M U S T   b e   l o c a l   t o   k e e p   t h i s   f i l e   f r o m   r e s a v i n g   i t s e l f   a d - n a u s e u m ,   m e s s i n g   u p   t h e   g i t   r e p o *  . / . q       0 0 �� 1�� 0 numargs numArgs 1 �� 2�� 0 pasteme pasteMe 2 �� 3�� 0 dothis doThis 3 ������ 0 	tmpdofile 	tmpDoFile��   /  4 5 4 q       6 6 �� 7�� 0 howmanystatas howManyStatas 7 �� 8�� 0 	thestatas 	theStatas 8 �� 9�� 0 thestataname theStataName 9 ������ "0 thestataversion theStataVersion��   5  : ; : q       < < �� =�� 0 theolddelims theOldDelims = �� >�� &0 thesplitosversion theSplitOSVersion > �� ?�� 0 osmajor osMajor ? ������ 0 osminor osMinor��   ;  @ A @ q       B B ������ $0 defaulttmpdofile defaultTmpDoFile��   A  C D C r      E F E m      G G � H H  f e e d S t a t a . d o F o      ���� $0 defaulttmpdofile defaultTmpDoFile D  I J I q     K K ������ 0 uiok UIOK��   J  L M L l   �� N O��   N . ( first check that UI scripting will work    O � P P P   f i r s t   c h e c k   t h a t   U I   s c r i p t i n g   w i l l   w o r k M  Q R Q l   �� S T��   S K E this test tests if UI elements are activated from the calling source    T � U U �   t h i s   t e s t   t e s t s   i f   U I   e l e m e n t s   a r e   a c t i v a t e d   f r o m   t h e   c a l l i n g   s o u r c e R  V W V Q    ' X Y Z X O     [ \ [ Z     ] ^�� _ ] 1    ��
�� 
uien ^ r     ` a ` m    ��
�� boovtrue a o      ���� 0 uiok UIOK��   _ r     b c b m    ��
�� boovfals c o      ���� 0 uiok UIOK \ m     d d�                                                                                  sevs  alis    V  	Tucholsky                  �3�BD ����System Events.app                                              �����3�        ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��   Y R      �� e��
�� .ascrerr ****      � **** e o      ���� 0 errmsg errMsg��   Z r   $ ' f g f m   $ %��
�� boovfals g o      ���� 0 uiok UIOK W  h i h l  ( (��������  ��  ��   i  j k j l  ( (�� l m��   l G A macOS 11 seems too have messed with the UI elements enabled item    m � n n �   m a c O S   1 1   s e e m s   t o o   h a v e   m e s s e d   w i t h   t h e   U I   e l e m e n t s   e n a b l e d   i t e m k  o p o l  ( (�� q r��   q E ? I'm not sure anymore how to check to see if things can be sent    r � s s ~   I ' m   n o t   s u r e   a n y m o r e   h o w   t o   c h e c k   t o   s e e   i f   t h i n g s   c a n   b e   s e n t p  t u t l   ( (�� v w��   v m g	tell application "Finder"		if (version as number) > 10 then			set UIOK to true		end if	end tell
	    w � x x �  	 t e l l   a p p l i c a t i o n   " F i n d e r "  	 	 i f   ( v e r s i o n   a s   n u m b e r )   >   1 0   t h e n  	 	 	 s e t   U I O K   t o   t r u e  	 	 e n d   i f  	 e n d   t e l l 
 	 u  y�� y l  (i z { | z Z   (i } ~��  } H   ( * � � o   ( )���� 0 uiok UIOK ~ k   - � � �  � � � r   - A � � � n  - 2 � � � I   . 2�������� 0 getosxversion getOSXversion��  ��   �  f   - . � J       � �  � � � o      ���� 0 osmajor osMajor �  ��� � o      ���� 0 osminor osMinor��   �  ��� � O   B � � � � k   F � � �  � � � I  F K������
�� .miscactvnull��� ��� null��  ��   �  ��� � Z   L � � ��� � � l  L O ����� � @   L O � � � o   L M���� 0 osmajor osMajor � m   M N���� ��  ��   � k   R s � �  � � � O  R c � � � I  Z b�� ���
�� .miscmvisnull���     **** � 4   Z ^�� �
�� 
xppa � m   \ ] � � � � � * P r i v a c y _ A c c e s s i b i l i t y��   � l  R W ����� � 5   R W�� ���
�� 
xppb � m   T U � � � � � X c o m . a p p l e . s e t t i n g s . P r i v a c y S e c u r i t y . e x t e n s i o n
�� kfrmID  ��  ��   �  ��� � I  d s�� � �
�� .sysodlogaskr        TEXT � m   d g � � � � �8 W h e n   S y s t e m   S e t t i n g s   o p e n s   t o   t h e   A c c e s s i b i l i t y   p o r t i o n   o f   t h e   P r i v a c y   &   S e c u r i t y   t a b ,   c l i c k   o n   t h e   S y s t e m   E v e n t s   t o g g l e   b u t t o n   e v e n   i f   i t   a p p e a r s   t o   b e   o n . � �� ���
�� 
btns � J   j o � �  ��� � m   j m � � � � �  O K��  ��  ��  ��   � l  v � � � � � Z   v � � ��� � � F   v � � � � l  v { ����� � B   v { � � � o   v w���� 0 osmajor osMajor � m   w z���� 
��  ��   � l  ~ � ����� � B   ~ � � � � o   ~ ���� 0 osminor osMinor � m    ����� 	��  ��   � k   � � � �  � � � r   � � � � � 4   � ��� �
�� 
xppb � m   � � � � � � � H c o m . a p p l e . p r e f e r e n c e . u n i v e r s a l a c c e s s � 1   � ���
�� 
xpcp �  ��� � I  � ��� � �
�� .sysodlogaskr        TEXT � m   � � � � � � � � W h e n   S y s t e m   P r e f e r e n c e s   o p e n s ,   b e   s u r e   t h a t   ' E n a b l e   a c c e s s   f o r   a s s i s t i v e   d e v i c e s '   i s   c h e c k e d ,   t h e n   t r y   a g a i n . � �� ���
�� 
btns � J   � � � �  ��� � m   � � � � � � �  O K��  ��  ��  ��   � k   � � � �  � � � I  � ��� ���
�� .miscmvisnull���     **** � n   � � � � � 4   � ��� �
�� 
xppa � m   � � � � � � � * P r i v a c y _ A c c e s s i b i l i t y � 5   � ��� ���
�� 
xppb � m   � � � � � � � : c o m . a p p l e . p r e f e r e n c e . s e c u r i t y
�� kfrmID  ��   �  ��� � Z   � � � ��� � � l  � � ����� � B   � � � � � o   � ����� 0 osmajor osMajor � m   � ����� 
��  ��   � l  � � � � � � I  � ��� � �
�� .sysodlogaskr        TEXT � m   � � � � � � �� W h e n   t h e   S e c u r i t y   &   P r i v a c y   p r e f e r e n c e   p a n e   o p e n s ,   c l i c k   t h e   l o c k   t o   a l l o w   c h a n g e s ,   e n t e r   y o u r   l o g i n   p a s s w o r d ,   a n d   c h e c k   t h e   c h e c k b o x   n e x t   t o   y o u r   v e r s i o n   o f   E m a c s .   I f   E m a c s   d o e s   n o t   a p p e a r   i n   t h e   l i s t   o f   a p p s ,   q u i t   E m a c s ,   s t a r t   E m a c s ,   a n d   t r y   a g a i n .   W h e n   f i n i s h e d ,   t r y   s e n d i n g   t o   S t a t a   a g a i n .   Y o u   w i l l   g e t   a s k e d   a b o u t   E m a c s   c o n t r o l l i n g   S t a t a .   C l i c k   O K � �� ���
�� 
btns � J   � � � �  ��� � m   � � � � � � �  O K��  ��   �   os 10.10 thru 10.15    � � � � (   o s   1 0 . 1 0   t h r u   1 0 . 1 5��   � l  � � � � � � I  � ��� � �
�� .sysodlogaskr        TEXT � m   � � � � � � �2 W h e n   t h e   S e c u r i t y   &   P r i v a c y   p r e f e r e n c e   p a n e   o p e n s ,   s e l e c t   t h e   P r i v a c y   t a b ,   t h e n   s e l e c t   t h e   A c c e s s i b i l i t y   i t e m   a n d   b e   s u r e   y o u r   v e r s i o n   o f   E m a c s   i s   c h e c k e d .   T h e n   s e l e c t   t h e   A u t o m a t i o n   i t e m   a n d   b e   s u r e   t h a t   e v e r y t h i n g   i s   c h e c k e d   u n d e r   y o u r   v e r s i o n   o f   E m a c s .   W h e n   f i n i s h e d ,   t r y   a g a i n . � �� ���
�� 
btns � J   � � � �  ��� � m   � � � � � � �  O K��  ��   �   osMajor is 11 or 12    � � � � (   o s M a j o r   i s   1 1   o r   1 2��   �   osMajor < 13    � � � �    o s M a j o r   <   1 3��   � m   B C � ��                                                                                  sprf  alis    N  	Tucholsky                  �3�BD ����System Settings.app                                            �����3�        ����  
 cu             Applications  */:System:Applications:System Settings.app/  (  S y s t e m   S e t t i n g s . a p p   	 T u c h o l s k y  'System/Applications/System Settings.app   / ��  ��  ��    k   �i � �    l  � �����   ' ! check proper number of arguments    � B   c h e c k   p r o p e r   n u m b e r   o f   a r g u m e n t s  r   � � l  � �	����	 n   � �

 1   � ���
�� 
leng o   � ����� 0 args  ��  ��   o      ���� 0 numargs numArgs  Q   �H k   �;  r   � � n   � � 4   � ���
�� 
cobj m   � �����  o   � ����� 0 args   o      ���� 0 dothis doThis  Z   ����� H   � E   � J   �	   !"! m   � �## �$$  c o m m a n d" %&% m   � �'' �((  m e n u& )*) m   �++ �,,  d o f i l e* -�- m  .. �//  i n c l u d e�   o  	
�~�~ 0 dothis doThis n 010 I  �}�|�{�} 0 badfirstarg badFirstArg�|  �{  1  f  ��  ��   2�z2 Z  ;34�y53 ?  676 o  �x�x 0 numargs numArgs7 m  �w�w 4 k  588 9:9 r  %;<; n  #=>= 4   #�v?
�v 
cobj? m  !"�u�u > o   �t�t 0 args  < o      �s�s 0 	tmpdofile 	tmpDoFile: @�r@ Z  &5AB�q�pA =  &+CDC o  &'�o�o 0 	tmpdofile 	tmpDoFileD m  '*EE �FF  B r  .1GHG o  ./�n�n $0 defaulttmpdofile defaultTmpDoFileH o      �m�m 0 	tmpdofile 	tmpDoFile�q  �p  �r  �y  5 r  8;IJI o  89�l�l $0 defaulttmpdofile defaultTmpDoFileJ o      �k�k 0 	tmpdofile 	tmpDoFile�z   R      �j�i�h
�j .ascrerr ****      � ****�i  �h   l CHKLMK n CHNON I  DH�g�f�e�g 0 badfirstarg badFirstArg�f  �e  O  f  CDL   no arguments   M �PP    n o   a r g u m e n t s QRQ l II�d�c�b�d  �c  �b  R STS l II�aUV�a  U U O grab clipboard, strip totally blank lines, to check if there is anything to do   V �WW �   g r a b   c l i p b o a r d ,   s t r i p   t o t a l l y   b l a n k   l i n e s ,   t o   c h e c k   i f   t h e r e   i s   a n y t h i n g   t o   d oT XYX l II�`Z[�`  Z 9 3   Aside: perhaps this should be on the emacs side?   [ �\\ f       A s i d e :   p e r h a p s   t h i s   s h o u l d   b e   o n   t h e   e m a c s   s i d e ?Y ]^] l II�__`�_  _ X R   for now it will stay here... could be wrong behavior, plus it is simpler to do    ` �aa �       f o r   n o w   i t   w i l l   s t a y   h e r e . . .   c o u l d   b e   w r o n g   b e h a v i o r ,   p l u s   i t   i s   s i m p l e r   t o   d o  ^ bcb l II�^de�^  d       in Applescript (!)   e �ff .           i n   A p p l e s c r i p t   ( ! )c ghg r  IUiji n ISklk I  JS�]m�\�] "0 stripblanklines stripBlankLinesm n�[n I JO�Z�Y�X
�Z .JonsgClp****    ��� null�Y  �X  �[  �\  l  f  IJj o      �W�W 0 pasteme pasteMeh opo Z  Vxqr�V�Uq =  V[sts o  VW�T�T 0 pasteme pasteMet m  WZuu �vv  r O  ^twxw I ds�Syz
�S .sysodlogaskr        TEXTy m  dg{{ �|| , N o t h i n g   t o   s e n d   S t a t a !z �R}�Q
�R 
btns} J  jo~~ �P m  jm�� ���  C a n c e l�P  �Q  x m  ^a���                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �V  �U  p ��� l yy�O�N�M�O  �N  �M  � ��� l yy�L���L  � \ V in the best of worlds, it would be possible to allow looping through the instances of   � ��� �   i n   t h e   b e s t   o f   w o r l d s ,   i t   w o u l d   b e   p o s s i b l e   t o   a l l o w   l o o p i n g   t h r o u g h   t h e   i n s t a n c e s   o f� ��� l yy�K���K  � 5 /   Stata to send the same code to each instance   � ��� ^       S t a t a   t o   s e n d   t h e   s a m e   c o d e   t o   e a c h   i n s t a n c e� ��� O  y���� r  }���� l }���J�I� 6 }���� 2  }��H
�H 
prcs� E  ����� 1  ���G
�G 
pnam� m  ���� ��� 
 S t a t a�J  �I  � o      �F�F 0 	thestatas 	theStatas� m  yz���                                                                                  sevs  alis    V  	Tucholsky                  �3�BD ����System Events.app                                              �����3�        ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  � ��� l ���E�D�C�E  �D  �C  � ��� r  ����� l ����B�A� I ���@��?
�@ .corecnte****       ****� o  ���>�> 0 	thestatas 	theStatas�?  �B  �A  � o      �=�= 0 howmanystatas howManyStatas� ��� Z  �����<�� = ����� o  ���;�; 0 howmanystatas howManyStatas� m  ���:�:  � O  ����� k  ���� ��� I ���9�8�7
�9 .sysobeepnull��� ��� long�8  �7  � ��6� I ���5��
�5 .sysodlogaskr        TEXT� m  ���� ��� " N o   S t a t a   r u n n i n g !� �4��3
�4 
btns� J  ���� ��2� m  ���� ���  C a n c e l�2  �3  �6  � m  �����                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �<  � k  ���� ��� Z  �����1�0� ?  ����� o  ���/�/ 0 howmanystatas howManyStatas� m  ���.�. � k  ���� ��� O  ����� I ���-��
�- .sysodlogaskr        TEXT� m  ���� ��� @ n o t h i n g   f o r   m u l t i p l e   s t a t a ' s   y e t� �,��+
�, 
btns� J  ���� ��*� m  ���� ���  C a n c e l�*  �+  � m  �����                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  � ��� l ���)���)  � : 4 Stata can use the same name for different processes   � ��� h   S t a t a   c a n   u s e   t h e   s a m e   n a m e   f o r   d i f f e r e n t   p r o c e s s e s� ��� l ���(���(  � J D so... the it is impossible to cycle through Stata processes by name   � ��� �   s o . . .   t h e   i t   i s   i m p o s s i b l e   t o   c y c l e   t h r o u g h   S t a t a   p r o c e s s e s   b y   n a m e� ��'� l  ���&���&  � � � tell application "System Events"					set theStatas to (the file of every process whose name contains "Stata")				 end tell				repeat with aStata in theStatas				end repeat
				   � ���j   t e l l   a p p l i c a t i o n   " S y s t e m   E v e n t s "  	 	 	 	 	 s e t   t h e S t a t a s   t o   ( t h e   f i l e   o f   e v e r y   p r o c e s s   w h o s e   n a m e   c o n t a i n s   " S t a t a " )  	 	 	 	   e n d   t e l l  	 	 	 	 r e p e a t   w i t h   a S t a t a   i n   t h e S t a t a s  	 	 	 	 e n d   r e p e a t 
 	 	 	 	�'  �1  �0  � ��� l ���%���%  � : 4 know there is exactly one instance of Stata running   � ��� h   k n o w   t h e r e   i s   e x a c t l y   o n e   i n s t a n c e   o f   S t a t a   r u n n i n g� ��� l ���$���$  �   can finally get to work   � ��� 0   c a n   f i n a l l y   g e t   t o   w o r k� ��#� O  ����� r  ����� l ����"�!� l ���� �� n  ����� 1  ���
� 
pnam� l ������ n  ����� 4 ����
� 
cobj� m  ���� � o  ���� 0 	thestatas 	theStatas�  �  �   �  �"  �!  � o      �� 0 thestataname theStataName� m  �����                                                                                  sevs  alis    V  	Tucholsky                  �3�BD ����System Events.app                                              �����3�        ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  �#  � ��� l ������  �  �  � ��� l ������  � $  Stata *must* be made active		   � ��� <   S t a t a   * m u s t *   b e   m a d e   a c t i v e 	 	� � � O  �> k  �=  r  � l ��� n  �	
	 1  ��
� 
bnid
 l ���� 4  ���
� 
prcs o  ���� 0 thestataname theStataName�  �  �  �   o      �� "0 thestataversion theStataVersion  r   n  1  	�
� 
txdl 1  	�

�
 
ascr o      �	�	 0 theolddelims theOldDelims  r   J   � m   �  c o m . s t a t a . s t a t a�   n      1  �
� 
txdl 1  �
� 
ascr  r  &  l $!��! n  $"#" 4 $�$
� 
citm$ m  "#����# o  �� "0 thestataversion theStataVersion�  �    o      � �  "0 thestataversion theStataVersion %&% r  '0'(' o  '(���� 0 theolddelims theOldDelims( n     )*) 1  +/��
�� 
txdl* 1  (+��
�� 
ascr& +��+ r  1=,-, m  12��
�� boovtrue- n      ./. 1  8<��
�� 
pisf/ 4  28��0
�� 
prcs0 l 671����1 o  67���� 0 thestataname theStataName��  ��  ��   m  ��22�                                                                                  sevs  alis    V  	Tucholsky                  �3�BD ����System Events.app                                              �����3�        ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��    343 l ??��������  ��  ��  4 5��5 Z  ?i67��86 = ?D9:9 o  ?@���� 0 dothis doThis: m  @C;; �<<  c o m m a n d7 Z  G^=>��?= @  GJ@A@ o  GH���� "0 thestataversion theStataVersionA m  HI���� > n MTBCB I  NT��D����  0 dostatacommand doStataCommandD EFE o  NO���� 0 thestataname theStataNameF G��G o  OP���� 0 pasteme pasteMe��  ��  C  f  MN��  ? n W^HIH I  X^��J���� 0 pastetmpstata pasteTmpStataJ KLK o  XY���� 0 thestataname theStataNameL M��M o  YZ���� 0 pasteme pasteMe��  ��  I  f  WX��  8 n aiNON I  bi��P���� 0 dotmpdofile doTmpDofileP QRQ o  bc���� 0 thestataname theStataNameR STS o  cd���� 0 	tmpdofile 	tmpDoFileT U��U o  de���� 0 dothis doThis��  ��  O  f  ab��   { ' !- from test of UI being turned on    | �VV B -   f r o m   t e s t   o f   U I   b e i n g   t u r n e d   o n��    WXW l     ��������  ��  ��  X YZY i    [\[ I      �������� 0 badfirstarg badFirstArg��  ��  \ O     ]^] I   ��_`
�� .sysodlogaskr        TEXT_ m    aa �bb \ T h e   f i r s t   a r g u m e n t   m u s t   b e   " c o m m a n d "   o r   " m e n u "` ��c��
�� 
btnsc J    	dd e��e m    ff �gg  C a n c e l��  ��  ^ m     hh�                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  Z iji l     ��������  ��  ��  j klk i    mnm I      ��o���� 0 dotmpdofile doTmpDofileo pqp o      ���� 0 	stataname 	stataNameq rsr o      ���� 0 	tmpdofile 	tmpDoFiles t��t o      ���� 0 dowhat doWhat��  ��  n k    ,uu vwv l     ��xy��  x K E if multiple instances ever work, be sure this gets written just once   y �zz �   i f   m u l t i p l e   i n s t a n c e s   e v e r   w o r k ,   b e   s u r e   t h i s   g e t s   w r i t t e n   j u s t   o n c ew {|{ q      }} ��~�� 
0 tmpdir  ~ ������ "0 stupidapplefile stupidAppleFile��  | � l     ������  � X R need to change this, because it changes the working directory in Stata on the Mac   � ��� �   n e e d   t o   c h a n g e   t h i s ,   b e c a u s e   i t   c h a n g e s   t h e   w o r k i n g   d i r e c t o r y   i n   S t a t a   o n   t h e   M a c� ��� r     ��� I    �����
�� .sysoexecTEXT���     TEXT� m     �� ��� 8 g e t c o n f   D A R W I N _ U S E R _ T E M P _ D I R��  � o      ���� 
0 tmpdir  � ��� r    ��� l   ������ b    ��� o    	���� 
0 tmpdir  � o   	 
���� 0 	tmpdofile 	tmpDoFile��  ��  � o      ���� 0 	tmpdofile 	tmpDoFile� ��� l   ������  � < 6 need applescript-style file name to write to the file   � ��� l   n e e d   a p p l e s c r i p t - s t y l e   f i l e   n a m e   t o   w r i t e   t o   t h e   f i l e� ��� r    ��� 4    ���
�� 
psxf� o    ���� 0 	tmpdofile 	tmpDoFile� o      ���� "0 stupidapplefile stupidAppleFile� ��� Q    g���� k    C�� ��� I   ����
�� .rdwropenshor       file� o    ���� "0 stupidapplefile stupidAppleFile� �����
�� 
perm� m    ��
�� boovtrue��  � ��� I    '����
�� .rdwrseofnull���     ****� o     !���� "0 stupidapplefile stupidAppleFile� �����
�� 
set2� m   " #����  ��  � ��� I  ( 5����
�� .rdwrwritnull���     ****� l  ( -������ I  ( -������
�� .JonsgClp****    ��� null��  ��  ��  ��  � ����
�� 
refn� o   . /���� "0 stupidapplefile stupidAppleFile� �����
�� 
as  � m   0 1��
�� 
utf8��  � ��� I  6 =����
�� .rdwrwritnull���     ****� o   6 7��
�� 
ret � �����
�� 
refn� o   8 9���� "0 stupidapplefile stupidAppleFile��  � ���� I  > C�����
�� .rdwrclosnull���     ****� o   > ?���� "0 stupidapplefile stupidAppleFile��  ��  � R      ������
�� .ascrerr ****      � ****��  ��  � k   K g�� ��� I  K P�����
�� .rdwrclosnull���     ****� o   K L���� "0 stupidapplefile stupidAppleFile��  � ���� O   Q g��� I  W f����
�� .sysodlogaskr        TEXT� m   W Z�� ��� L H a d   t r o u b l e   w i t h   t h e   t e m p o r a r y   d o - f i l e� �����
�� 
btns� J   ] b�� ���� m   ] `�� ���  C a n c e l��  ��  � m   Q T���                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ��  � ��� l  h h������  � #  applescript really is a pita   � ��� :   a p p l e s c r i p t   r e a l l y   i s   a   p i t a� ��� O   h w��� I  q v������
�� .miscactvnull��� ��� null��  ��  � 4   h n���
�� 
capp� o   l m���� 0 	stataname 	stataName� ���� Z   x,������ =   x }��� o   x y���� 0 dowhat doWhat� m   y |�� ���  m e n u� Q   � ����� I   � �������� 0 	doviamenu 	doViaMenu� ���� o   � ����� 0 	stataname 	stataName��  ��  � R      ������
�� .ascrerr ****      � ****��  ��  � k   � ��� ��� I   � ����~� "0 createmenuitems createMenuItems� ��� o   � ��}�} 0 	stataname 	stataName� ��|� o   � ��{�{ 0 	tmpdofile 	tmpDoFile�|  �~  � ��� l  � ��z���z  � 8 2 need to be sure the menu item exists on first try   � ��� d   n e e d   t o   b e   s u r e   t h e   m e n u   i t e m   e x i s t s   o n   f i r s t   t r y� ��� I  � ��y��x
�y .sysodelanull��� ��� nmbr� m   � ��w�w �x  � ��v� Q   � ����� I   � ��u��t�u 0 	doviamenu 	doViaMenu� ��s� o   � ��r�r 0 	stataname 	stataName�s  �t  � R      �q�p�o
�q .ascrerr ****      � ****�p  �o  � O   � ���� I  � ��n��
�n .sysodlogaskr        TEXT� m   � ��� ��� v S o m e t h i n g   w e n t   w r o n g . . .   i s   y o u r   s t a t a   v e r s i o n   s e t   p r o p e r l y ?� �m��l
�m 
btns� J   � ���  �k  m   � � �  C a n c e l�k  �l  � m   � ��                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �v  ��  � l  �, Z   �,�j	 =   � �

 o   � ��i�i 0 dowhat doWhat m   � � �  i n c l u d e Q   � � n  � � I   � ��h�g�h 0 pastetmpstata pasteTmpStata  o   � ��f�f 0 	stataname 	stataName �e b   � � m   � � �  i n c l u d e   o   � ��d�d 0 	tmpdofile 	tmpDoFile�e  �g    f   � � R      �c�b�a
�c .ascrerr ****      � ****�b  �a   O   � � I  � ��`
�` .sysodlogaskr        TEXT m   � � �   \ H a d   t r o u b l e   r u n n i n g   v i a   t e m p o r a r y   i n c l u d e   f i l e �_!�^
�_ 
btns! J   � �"" #�]# m   � �$$ �%%  C a n c e l�]  �^   m   � �&&�                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �j  	 l  ,'()' Q   ,*+,* k  -- ./. l �\01�\  0 = 7 'open' changes directory as an unavoidable side-effect   1 �22 n   ' o p e n '   c h a n g e s   d i r e c t o r y   a s   a n   u n a v o i d a b l e   s i d e - e f f e c t/ 343 l �[56�[  5   open stupidAppleFile   6 �77 *   o p e n   s t u p i d A p p l e F i l e4 8�Z8 n 9:9 I  �Y;�X�Y 0 pastetmpstata pasteTmpStata; <=< o  �W�W 0 	stataname 	stataName= >�V> b  
?@? m  AA �BB  d o  @ o  	�U�U 0 	tmpdofile 	tmpDoFile�V  �X  :  f  �Z  + R      �T�S�R
�T .ascrerr ****      � ****�S  �R  , O  ,CDC I +�QEF
�Q .sysodlogaskr        TEXTE m  GG �HH R H a d   t r o u b l e   r u n n i n g   v i a   t e m p o r a r y   d o - f i l eF �PI�O
�P 
btnsI J  "'JJ K�NK m  "%LL �MM  C a n c e l�N  �O  D m  NN�                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  (   doing via dofile   ) �OO "   d o i n g   v i a   d o f i l e I C doing via dofile or include--- needs fixing for multiple instances    �PP �   d o i n g   v i a   d o f i l e   o r   i n c l u d e - - -   n e e d s   f i x i n g   f o r   m u l t i p l e   i n s t a n c e s��  l QRQ l     �M�L�K�M  �L  �K  R STS i    UVU I      �JW�I�J 0 	doviamenu 	doViaMenuW X�HX o      �G�G 0 	stataname 	stataName�H  �I  V O     /YZY Z    .[\�F�E[ 1    �D
�D 
uien\ O    *]^] I   )�C_�B
�C .prcsclicnull��� ��� uiel_ n    %`a` 4   " %�Ab
�A 
menIb m   # $cc �dd  r u n   t m p   f i l ea n    "efe 4    "�@g
�@ 
menEg m     !�?�? f n    hih 4    �>j
�> 
menIj m    kk �ll & A p p l e s c r i p t   h e l p e r si n    mnm 4    �=o
�= 
menEo m    �<�< n n    pqp 4    �;r
�; 
mbrir m    ss �tt  U s e rq 4    �:u
�: 
mbaru m    �9�9 �B  ^ 4    �8v
�8 
pcapv o    �7�7 0 	stataname 	stataName�F  �E  Z m     ww�                                                                                  sevs  alis    V  	Tucholsky                  �3�BD ����System Events.app                                              �����3�        ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  T xyx l     �6�5�4�6  �5  �4  y z{z i    |}| I      �3~�2�3 "0 createmenuitems createMenuItems~ � o      �1�1 0 	stataname 	stataName� ��0� o      �/�/ 0 	tmpdofile 	tmpDoFile�0  �2  } O     G��� Z    F���.�-� 1    �,
�, 
uien� O    B��� k    A�� ��� l   �+���+  � * $ get the command window to the front   � ��� H   g e t   t h e   c o m m a n d   w i n d o w   t o   t h e   f r o n t� ��� I   #�*��)
�* .prcsclicnull��� ��� uiel� n   ��� 4    �(�
�( 
menI� m    �� ���  C o m m a n d� n    ��� 4    �'�
�' 
menE� m    �� ���  W i n d o w� n   ��� 4    �&�
�& 
mbri� m    �� ���  W i n d o w� 4    �%�
�% 
mbar� m    �$�$ �)  � ��� I  $ +�#��"
�# .prcskprsnull���     ctxt� b   $ '��� m   $ %�� ��� r w i n d o w   m e n u   a p p e n d   s u b m e n u   " s t U s e r "   " A p p l e s c r i p t   h e l p e r s "� o   % &�!
�! 
ret �"  � ��� I  , 7� ��
�  .prcskprsnull���     ctxt� b   , 3��� b   , 1��� b   , /��� m   , -�� ��� � w i n d o w   m e n u   a p p e n d   i t e m   " A p p l e s c r i p t   h e l p e r s "   " r u n   t m p   f i l e "   " d o  � o   - .�� 0 	tmpdofile 	tmpDoFile� m   / 0�� ���  "� o   1 2�
� 
ret �  � ��� I  8 A���
� .prcskprsnull���     ctxt� b   8 =��� m   8 ;�� ��� & w i n d o w   m e n u   r e f r e s h� o   ; <�
� 
ret �  �  � 4    ��
� 
pcap� o    �� 0 	stataname 	stataName�.  �-  � m     ���                                                                                  sevs  alis    V  	Tucholsky                  �3�BD ����System Events.app                                              �����3�        ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  { ��� l     ����  �  �  � ��� i    ��� I      ����  0 dostatacommand doStataCommand� ��� o      �� 0 	stataname 	stataName� ��� o      �� 0 
thecommand 
theCommand�  �  � Q     *���� w    ��� l   ���� O    ��� k    �� ��� l   ����  �  		activate   � ���  	 a c t i v a t e� ��� I   ���
� .STscDoCanull���     ctxt� o    �� 0 
thecommand 
theCommand� �
��	�
 0 addtoreview addToReview� m    �
� boovtrue�	  �  � 4    	��
� 
capp� o    �� 0 	stataname 	stataName�   needed to compile   � ��� $   n e e d e d   t o   c o m p i l e��                                                                                  S5x8  alis    ^  	Tucholsky                  �3�BD ����StataMP.app                                                    �����`�        ����  
 cu             Stata18   </:Applications:AAApplications:MathTools:Stata18:StataMP.app/    S t a t a M P . a p p   	 T u c h o l s k y  9Applications/AAApplications/MathTools/Stata18/StataMP.app   / ��  � R      ���
� .ascrerr ****      � ****�  �  � O    *��� I    )���
� .sysodlogaskr        TEXT� m     !�� ��� l H a d   t r o u b l e   p a s s i n g   c o m m a n d ( s )   t o   S t a t a   c o m m a n d   w i n d o w� ��� 
� 
btns� J   " %�� ���� m   " #�� ���  C a n c e l��  �   � m    ���                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  � ��� l     ��������  ��  ��  � ��� l     ��������  ��  ��  � ��� i    ��� I      ������� 0 pastetmpstata pasteTmpStata� ��� o      ���� 0 	stataname 	stataName� ���� o      ���� 0 pasteme pasteMe��  ��  � k     ��� ��� q      �� ������ 0 oldclipboard oldClipBoard��  � ��� r     ��� I    ������
�� .JonsgClp****    ��� null��  ��  � o      ���� 0 oldclipboard oldClipBoard� ��� Q    �� � k    h  I   ����
�� .JonspClpnull���     **** l   ���� c     o    ���� 0 pasteme pasteMe m    ��
�� 
ctxt��  ��  ��   	��	 O    h

 Z    g���� 1    ��
�� 
uien O    c k   % b  I  % 6����
�� .prcsclicnull��� ��� uiel n  % 2 4   / 2��
�� 
menI m   0 1 �  C o m m a n d n   % / 4   , /��
�� 
menE m   - . �  W i n d o w n  % , 4   ) ,�� 
�� 
mbri  m   * +!! �""  W i n d o w 4   % )��#
�� 
mbar# m   ' (���� ��   $%$ I  7 <��&��
�� .sysodelanull��� ��� nmbr& m   7 8'' ?ə�������  % ()( I  = T��*��
�� .prcsclicnull��� ��� uiel* n  = P+,+ 4   K P��-
�� 
menI- l 	 L O.����. m   L O// �00 
 P a s t e��  ��  , n   = K121 4   F K��3
�� 
menE3 m   G J44 �55  E d i t2 n  = F676 4   A F��8
�� 
mbri8 m   B E99 �::  E d i t7 4   = A��;
�� 
mbar; m   ? @���� ��  ) <=< l  U U��>?��  > > 8 added delay when seeing odd behavior on machine at work   ? �@@ p   a d d e d   d e l a y   w h e n   s e e i n g   o d d   b e h a v i o r   o n   m a c h i n e   a t   w o r k= ABA l  U U��CD��  C U O it seems that a delay of under 0.2 seconds makes things weird on fast machines   D �EE �   i t   s e e m s   t h a t   a   d e l a y   o f   u n d e r   0 . 2   s e c o n d s   m a k e s   t h i n g s   w e i r d   o n   f a s t   m a c h i n e sB FGF I  U Z��H��
�� .sysodelanull��� ��� nmbrH m   U VII ?ə�������  G J��J I  [ b��K��
�� .prcskprsnull���     ctxtK o   [ ^��
�� 
ret ��  ��   4    "��L
�� 
pcapL o     !���� 0 	stataname 	stataName��  ��   m    MM�                                                                                  sevs  alis    V  	Tucholsky                  �3�BD ����System Events.app                                              �����3�        ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  ��    R      ������
�� .ascrerr ****      � ****��  ��   k   p �NN OPO I  p u��Q��
�� .JonspClpnull���     ****Q o   p q���� 0 oldclipboard oldClipBoard��  P R��R O   v �STS I  | ���UV
�� .sysodlogaskr        TEXTU m   | WW �XX V H a d   t r o u b l e   p a s t i n g   t o   S t a t a   c o m m a n d   w i n d o wV ��Y��
�� 
btnsY J   � �ZZ [��[ m   � �\\ �]]  C a n c e l��  ��  T m   v y^^�                                                                                  MACS  alis    :  	Tucholsky                  �3�BD ����
Finder.app                                                     �����3�        ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ��  � _��_ I  � ���`��
�� .JonspClpnull���     ****` o   � ����� 0 oldclipboard oldClipBoard��  ��  � aba l     ��������  ��  ��  b cdc l     ��������  ��  ��  d efe i    ghg I      ��i���� "0 stripblanklines stripBlankLinesi j��j o      ���� 0 thetext theText��  ��  h k     ikk lml q      nn ��o�� 0 theparas theParaso ��p�� 0 thepara theParap ��q�� 0 achar aCharq ��r�� 0 thecleanstuff theCleanStuffr ������ 0 eraseme  ��  m sts r     uvu l    w����w n     xyx 2   ��
�� 
cpary o     ���� 0 thetext theText��  ��  v o      ���� 0 theparas theParast z{z r    
|}| J    ����  } o      ���� 0 thecleanstuff theCleanStuff{ ~~ X    ^����� k    Y�� ��� r    ��� m    ��
�� boovtrue� o      ���� 0 eraseme  � ��� X    I����� Z   1 D������� H   1 8�� E  1 7��� J   1 5�� ��� m   1 2�� ���   � ���� m   2 3�� ���  	��  � o   5 6���� 0 achar aChar� k   ; @�� ��� r   ; >��� m   ; <��
�� boovfals� o      ���� 0 eraseme  � ����  S   ? @��  ��  ��  �� 0 achar aChar� n   " %��� 2   # %��
�� 
cha � o   " #���� 0 thepara thePara� ���� Z   J Y������� H   J L�� o   J K���� 0 eraseme  � r   O U��� l  O R������ c   O R��� o   O P���� 0 thepara thePara� m   P Q��
�� 
TEXT��  ��  � l     ������ n      ���  ;   S T� o   R S���� 0 thecleanstuff theCleanStuff��  ��  ��  ��  ��  �� 0 thepara thePara� o    ���� 0 theparas theParas ��� r   _ d��� o   _ `��
�� 
ret � l     ������ 1   ` c��
�� 
txdl��  ��  � ���� L   e i�� l  e h������ c   e h��� o   e f���� 0 thecleanstuff theCleanStuff� m   f g��
�� 
TEXT��  ��  ��  f ��� l     ��~�}�  �~  �}  � ��|� i     #��� I      �{�z�y�{ 0 getosxversion getOSXversion�z  �y  � l    C���� k     C�� ��� q      �� �x�w�x 0 oslist osList�w  � ��� r     ��� m     �� ���  .� l     ��v�u� 1    �t
�t 
txdl�v  �u  � ��� r    ��� n    ��� 2    �s
�s 
citm� l   ��r�q� n    ��� 1    �p
�p 
sisv� l   ��o�n� I   �m�l�k
�m .sysosigtsirr   ��� null�l  �k  �o  �n  �r  �q  � o      �j�j 0 oslist osList� ��� Y    /��i���h� r    *��� l   %��g�f� c    %��� n    #��� 4     #�e�
�e 
cobj� o   ! "�d�d 0 cnt  � o     �c�c 0 oslist osList� m   # $�b
�b 
long�g  �f  � n      ��� 4   & )�a�
�a 
cobj� o   ' (�`�` 0 cnt  � o   % &�_�_ 0 oslist osList�i 0 cnt  � m    �^�^ � l   ��]�\� n    ��� 1    �[
�[ 
leng� o    �Z�Z 0 oslist osList�]  �\  �h  � ��� Z   0 @���Y�X� =  0 5��� l  0 3��W�V� n   0 3��� 1   1 3�U
�U 
leng� o   0 1�T�T 0 oslist osList�W  �V  � m   3 4�S�S � s   8 <��� m   8 9�R�R  � l     ��Q�P� n      ���  ;   : ;� o   9 :�O�O 0 oslist osList�Q  �P  �Y  �X  � ��N� L   A C�� o   A B�M�M 0 oslist osList�N  � 3 - returns a list with major and minor versions   � ��� Z   r e t u r n s   a   l i s t   w i t h   m a j o r   a n d   m i n o r   v e r s i o n s�|       �L��������� �L  � 	�K�J�I�H�G�F�E�D�C
�K .aevtoappnull  �   � ****�J 0 badfirstarg badFirstArg�I 0 dotmpdofile doTmpDofile�H 0 	doviamenu 	doViaMenu�G "0 createmenuitems createMenuItems�F  0 dostatacommand doStataCommand�E 0 pastetmpstata pasteTmpStata�D "0 stripblanklines stripBlankLines�C 0 getosxversion getOSXversion� �B !�A�@�?
�B .aevtoappnull  �   � ****�A 0 args  �@   �>�=�<�;�:�9�8�7�6�5�4�3�2�1�0�/�> 0 args  �= 0 numargs numArgs�< 0 pasteme pasteMe�; 0 dothis doThis�: 0 	tmpdofile 	tmpDoFile�9 0 howmanystatas howManyStatas�8 0 	thestatas 	theStatas�7 0 thestataname theStataName�6 "0 thestataversion theStataVersion�5 0 theolddelims theOldDelims�4 &0 thesplitosversion theSplitOSVersion�3 0 osmajor osMajor�2 0 osminor osMinor�1 $0 defaulttmpdofile defaultTmpDoFile�0 0 uiok UIOK�/ 0 errmsg errMsg D G d�.�-�,�+�* ��)�(�' ��&�% ��$ ��# ��"�!� � �� � � � � � � � ��#'+.��E���u�{���������������;���
�. 
uien�- 0 errmsg errMsg�,  �+ 0 getosxversion getOSXversion
�* 
cobj
�) .miscactvnull��� ��� null�( 
�' 
xppb
�& kfrmID  
�% 
xppa
�$ .miscmvisnull���     ****
�# 
btns
�" .sysodlogaskr        TEXT�! 
�  	
� 
bool
� 
xpcp
� 
leng� � 0 badfirstarg badFirstArg�  
� .JonsgClp****    ��� null� "0 stripblanklines stripBlankLines
� 
prcs  
� 
pnam
� .corecnte****       ****
� .sysobeepnull��� ��� long
� 
bnid
� 
ascr
� 
txdl
� 
citm
� 
pisf�  0 dostatacommand doStataCommand� 0 pastetmpstata pasteTmpStata� 0 dotmpdofile doTmpDofile�?j�E�O � *�,E eE�Y fE�UW 
X  fE�O� �)j+ E[�k/E�Z[�l/E�ZO� �*j O�� &*���0 
*��/j UOa a a kvl Y o�a 	 �a a & !*�a /*a ,FOa a a kvl Y <*�a �0�a /j O�a  a a a kvl Y a a a  kvl UY��a !,E�O N��k/E�Oa "a #a $a %a &v� 
)j+ 'Y hO�k ��l/E�O�a (  �E�Y hY �E�W X ) )j+ 'O)*j *k+ +E�O�a ,  a - a .a a /kvl UY hO� *a 0-a 1[a 2,\Za 3@1E�UO�j 4E�O�j  !a - *j 5Oa 6a a 7kvl UY 4�k a - a 8a a 9kvl UOPY hO� ��k/a 2,E�UO� F*a 0�/a :,E�O_ ;a <,E�Oa =kv_ ;a <,FO�a >i/E�O�_ ;a <,FOe*a 0�/a ?,FUO�a @  �� )��l+ AY 	)��l+ BY 
)���m+ C� �\�
�	�� 0 badfirstarg badFirstArg�
  �	     ha�f�
� 
btns
� .sysodlogaskr        TEXT� � ���kvl U� �n���� 0 dotmpdofile doTmpDofile� ��   � �����  0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFile�� 0 dowhat doWhat�   ������������ 0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFile�� 0 dowhat doWhat�� 
0 tmpdir  �� "0 stupidapplefile stupidAppleFile &������������������������������������������������������$AGL
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
�� 
as  
�� 
utf8�� 
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
�� .sysodelanull��� ��� nmbr�� 0 pastetmpstata pasteTmpStata�-�j E�O��%E�O*�/E�O 0��el O��jl O*j ���� O��l O�j W #X  �j Oa  a a a kvl UO*a �/ *j UO�a   K *�k+ W <X  *��l+ Okj O *�k+ W X  a  a a a kvl UY e�a   1 )�a �%l+  W X  a  a !a a "kvl UY . )�a #�%l+  W X  a  a $a a %kvl U� ��V����	
���� 0 	doviamenu 	doViaMenu�� ����   ���� 0 	stataname 	stataName��  	 ���� 0 	stataname 	stataName
 w��������s����kc��
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
�� .prcsclicnull��� ��� uiel�� 0� ,*�,E $*�/ *�k/��/�k/��/�k/��/j 
UY hU� ��}�������� "0 createmenuitems createMenuItems�� ����   ������ 0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFile��   ������ 0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFile ��������������������������
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
�� .prcsclicnull��� ��� uiel
�� 
ret 
�� .prcskprsnull���     ctxt�� H� D*�,E <*�/ 1*�k/��/��/��/j 
O��%j O�%�%�%j Oa �%j UY hU� �����������  0 dostatacommand doStataCommand�� ����   ������ 0 	stataname 	stataName�� 0 
thecommand 
theCommand��   ������ 0 	stataname 	stataName�� 0 
thecommand 
theCommand ������������������
�� 
capp�� 0 addtoreview addToReview
�� .STscDoCanull���     ctxt��  ��  
�� 
btns
�� .sysodlogaskr        TEXT�� + �Z*�/ 	��el UW X  � ���kvl 
U� ����������� 0 pastetmpstata pasteTmpStata�� ����   ������ 0 	stataname 	stataName�� 0 pasteme pasteMe��   �������� 0 	stataname 	stataName�� 0 pasteme pasteMe�� 0 oldclipboard oldClipBoard ������M��������!������'��94/��������^W��\��
�� .JonsgClp****    ��� null
�� 
ctxt
�� .JonspClpnull���     ****
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
�� .prcsclicnull��� ��� uiel
�� .sysodelanull��� ��� nmbr
�� 
ret 
�� .prcskprsnull���     ctxt��  ��  
�� 
btns
�� .sysodlogaskr        TEXT�� �*j  E�O b��&j O� R*�,E J*�/ ?*�k/��/��/��/j O�j O*�k/�a /�a /�a /j O�j O_ j UY hUW #X  �j Oa  a a a kvl UO�j � ��h�������� "0 stripblanklines stripBlankLines�� ����   ���� 0 thetext theText��   �������������� 0 thetext theText�� 0 theparas theParas�� 0 thepara thePara�� 0 achar aChar�� 0 thecleanstuff theCleanStuff�� 0 eraseme   
������������������
�� 
cpar
�� 
kocl
�� 
cobj
�� .corecnte****       ****
�� 
cha 
�� 
TEXT
�� 
ret 
�� 
txdl�� j��-E�OjvE�O R�[��l kh eE�O )��-[��l kh ��lv� 
fE�OY h[OY��O� ��&�6FY h[OY��O�*�,FO��&  �������~�� 0 getosxversion getOSXversion��  �   �}�|�} 0 oslist osList�| 0 cnt   ��{�z�y�x�w�v�u
�{ 
txdl
�z .sysosigtsirr   ��� null
�y 
sisv
�x 
citm
�w 
leng
�v 
cobj
�u 
long�~ D�*�,FO*j �,�-E�O k��,Ekh ��/�&��/F[OY��O��,k  	j�6GY hO� ascr  ��ޭ