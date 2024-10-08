FasdUAS 1.101.10   ��   ��    k             l      ��  ��    ( " version 2.1.3 -  August 29, 2016      � 	 	 D   v e r s i o n   2 . 1 . 3   -     A u g u s t   2 9 ,   2 0 1 6     
  
 l      ��  ��    , & sends contents of clipboard to Stata      �   L   s e n d s   c o n t e n t s   o f   c l i p b o a r d   t o   S t a t a        l      ��  ��    U O allows running from command window, as a temporary do-file or via a menu item      �   �   a l l o w s   r u n n i n g   f r o m   c o m m a n d   w i n d o w ,   a s   a   t e m p o r a r y   d o - f i l e   o r   v i a   a   m e n u   i t e m        l      ��  ��    N H applescript bug: single bar in front of "include" causes compile error      �   �   a p p l e s c r i p t   b u g :   s i n g l e   b a r   i n   f r o n t   o f   " i n c l u d e "   c a u s e s   c o m p i l e   e r r o r        l      ��  ��    W Q args are: { "command" | "menu" | "dofile" || "include" } [ name-of-tmp-dofile ]      �   �   a r g s   a r e :   {   " c o m m a n d "   |   " m e n u "   |   " d o f i l e "   | |   " i n c l u d e "   }   [   n a m e - o f - t m p - d o f i l e   ]        i        !   I     �� "��
�� .aevtoappnull  �   � **** " o      ���� 0 args  ��   ! k    / # #  $ % $ l     �� & '��   &  - initializations    ' � ( ( " -   i n i t i a l i z a t i o n s %  ) * ) l     �� + ,��   + � �- NOTE: all vars using in the run handler MUST be local to keep this file from resaving itself ad-nauseum, messing up the git repo    , � - - -   N O T E :   a l l   v a r s   u s i n g   i n   t h e   r u n   h a n d l e r   M U S T   b e   l o c a l   t o   k e e p   t h i s   f i l e   f r o m   r e s a v i n g   i t s e l f   a d - n a u s e u m ,   m e s s i n g   u p   t h e   g i t   r e p o *  . / . q       0 0 �� 1�� 0 numargs numArgs 1 �� 2�� 0 pasteme pasteMe 2 �� 3�� 0 dothis doThis 3 ������ 0 	tmpdofile 	tmpDoFile��   /  4 5 4 q       6 6 �� 7�� 0 howmanystatas howManyStatas 7 �� 8�� 0 	thestatas 	theStatas 8 �� 9�� 0 thestataname theStataName 9 ������ "0 thestataversion theStataVersion��   5  : ; : q       < < �� =�� 0 theolddelims theOldDelims = �� >�� &0 thesplitosversion theSplitOSVersion > �� ?�� 0 osmajor osMajor ? ������ 0 osminor osMinor��   ;  @ A @ q       B B ������ $0 defaulttmpdofile defaultTmpDoFile��   A  C D C r      E F E m      G G � H H  f e e d S t a t a . d o F o      ���� $0 defaulttmpdofile defaultTmpDoFile D  I J I q     K K ������ 0 uiok UIOK��   J  L M L l   �� N O��   N / ) first check that UI scripting will work	    O � P P R   f i r s t   c h e c k   t h a t   U I   s c r i p t i n g   w i l l   w o r k 	 M  Q R Q Q    ' S T U S O     V W V Z     X Y�� Z X 1    ��
�� 
uien Y r     [ \ [ m    ��
�� boovtrue \ o      ���� 0 uiok UIOK��   Z r     ] ^ ] m    ��
�� boovfals ^ o      ���� 0 uiok UIOK W m     _ _�                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��   T R      �� `��
�� .ascrerr ****      � **** ` o      ���� 0 errmsg errMsg��   U r   $ ' a b a m   $ %��
�� boovfals b o      ���� 0 uiok UIOK R  c d c l  ( (��������  ��  ��   d  e f e l  ( (�� g h��   g G A macOS 11 seems too have messed with the UI elements enabled item    h � i i �   m a c O S   1 1   s e e m s   t o o   h a v e   m e s s e d   w i t h   t h e   U I   e l e m e n t s   e n a b l e d   i t e m f  j k j l  ( (�� l m��   l E ? I'm not sure anymore how to check to see if things can be sent    m � n n ~   I ' m   n o t   s u r e   a n y m o r e   h o w   t o   c h e c k   t o   s e e   i f   t h i n g s   c a n   b e   s e n t k  o p o l   ( (�� q r��   q m g	tell application "Finder"		if (version as number) > 10 then			set UIOK to true		end if	end tell
	    r � s s �  	 t e l l   a p p l i c a t i o n   " F i n d e r "  	 	 i f   ( v e r s i o n   a s   n u m b e r )   >   1 0   t h e n  	 	 	 s e t   U I O K   t o   t r u e  	 	 e n d   i f  	 e n d   t e l l 
 	 p  t�� t l  (/ u v w u Z   (/ x y�� z x H   ( * { { o   ( )���� 0 uiok UIOK y k   - � | |  } ~ } r   - A  �  n  - 2 � � � I   . 2�������� 0 getosxversion getOSXversion��  ��   �  f   - . � J       � �  � � � o      ���� 0 osmajor osMajor �  ��� � o      ���� 0 osminor osMinor��   ~  ��� � O   B � � � � k   F � � �  � � � I  F K������
�� .miscactvnull��� ��� null��  ��   �  ��� � Z   L � � ��� � � F   L W � � � l  L O ����� � B   L O � � � o   L M���� 0 osmajor osMajor � m   M N���� 
��  ��   � l  R U ����� � B   R U � � � o   R S���� 0 opminor opMinor � m   S T���� 	��  ��   � k   Z r � �  � � � r   Z b � � � 4   Z ^�� �
�� 
xppb � m   \ ] � � � � � H c o m . a p p l e . p r e f e r e n c e . u n i v e r s a l a c c e s s � 1   ^ a��
�� 
xpcp �  ��� � I  c r�� � �
�� .sysodlogaskr        TEXT � m   c f � � � � � � W h e n   S y s t e m   P r e f e r e n c e s   o p e n s ,   b e   s u r e   t h a t   ' E n a b l e   a c c e s s   f o r   a s s i s t i v e   d e v i c e s '   i s   c h e c k e d ,   t h e n   t r y   a g a i n . � �� ���
�� 
btns � J   i n � �  ��� � m   i l � � � � �  O K��  ��  ��  ��   � k   u � � �  � � � r   u  � � � 4   u {�� �
�� 
xppb � m   w z � � � � � : c o m . a p p l e . p r e f e r e n c e . s e c u r i t y � 1   { ~��
�� 
xpcp �  ��� � Z   � � � ��� � � l  � � ����� � B   � � � � � o   � ����� 0 osmajor osMajor � m   � ����� 
��  ��   � I  � ��� � �
�� .sysodlogaskr        TEXT � m   � � � � � � �j W h e n   t h e   S e c u r i t y   &   P r i v a c y   p r e f e r e n c e   p a n e   o p e n s ,   s e l e c t   t h e   P r i v a c y   t a b ,   t h e n   s e l e c t   t h e   A c c e s s i b i l i t y   i t e m   a n d   b e   s u r e   y o u r   v e r s i o n   o f   E m a c s   i s   c h e c k e d .   W h e n   f i n i s h e d ,   t r y   a g a i n . � �� ���
�� 
btns � J   � � � �  ��� � m   � � � � � � �  O K��  ��  ��   � I  � ��� � �
�� .sysodlogaskr        TEXT � m   � � � � � � �2 W h e n   t h e   S e c u r i t y   &   P r i v a c y   p r e f e r e n c e   p a n e   o p e n s ,   s e l e c t   t h e   P r i v a c y   t a b ,   t h e n   s e l e c t   t h e   A c c e s s i b i l i t y   i t e m   a n d   b e   s u r e   y o u r   v e r s i o n   o f   E m a c s   i s   c h e c k e d .   T h e n   s e l e c t   t h e   A u t o m a t i o n   i t e m   a n d   b e   s u r e   t h a t   e v e r y t h i n g   i s   c h e c k e d   u n d e r   y o u r   v e r s i o n   o f   E m a c s .   W h e n   f i n i s h e d ,   t r y   a g a i n . � �� ���
�� 
btns � J   � � � �  ��� � m   � � � � � � �  O K��  ��  ��  ��   � m   B C � ��                                                                                  sprf  alis    Z  	Tucholsky                      BD ����System Preferences.app                                         ����            ����  
 cu             Applications  -/:System:Applications:System Preferences.app/   .  S y s t e m   P r e f e r e n c e s . a p p   	 T u c h o l s k y  *System/Applications/System Preferences.app  / ��  ��  ��   z k   �/ � �  � � � l  � ��� � ���   � ' ! check proper number of arguments    � � � � B   c h e c k   p r o p e r   n u m b e r   o f   a r g u m e n t s �  � � � r   � � � � � l  � � ����� � n   � � � � � 1   � ���
�� 
leng � o   � ����� 0 args  ��  ��   � o      ���� 0 numargs numArgs �  � � � Q   � � � � � k   � � � �  � � � r   � � � � � n   � � � � � 4   � ��� �
�� 
cobj � m   � �����  � o   � ����� 0 args   � o      ���� 0 dothis doThis �  � � � Z   � � � ����� � H   � � � � E   � � � � � J   � � � �  � � � m   � � � � � � �  c o m m a n d �  � � � m   � � � � � � �  m e n u �  � � � m   � � � � � � �  d o f i l e �  ��� � m   � � � � � � �  i n c l u d e��   � o   � ����� 0 dothis doThis � n  � � � � � I   � ��������� 0 badfirstarg badFirstArg��  ��   �  f   � ���  ��   �  ��  Z   � ��� ?   � � o   � ����� 0 numargs numArgs m   � �����  k   � �  r   � �	
	 n   � � 4   � ���
�� 
cobj m   � �����  o   � ����� 0 args  
 o      ���� 0 	tmpdofile 	tmpDoFile �� Z   � ����� =   � � o   � ����� 0 	tmpdofile 	tmpDoFile m   � � �   r   � � o   � ����� $0 defaulttmpdofile defaultTmpDoFile o      ���� 0 	tmpdofile 	tmpDoFile��  ��  ��  ��   r   � � o   � ����� $0 defaulttmpdofile defaultTmpDoFile o      ���� 0 	tmpdofile 	tmpDoFile��   � R      ��~�}
� .ascrerr ****      � ****�~  �}   � l  n  I  �|�{�z�| 0 badfirstarg badFirstArg�{  �z    f     no arguments    �    n o   a r g u m e n t s �   l �y�x�w�y  �x  �w    !"! l �v#$�v  # U O grab clipboard, strip totally blank lines, to check if there is anything to do   $ �%% �   g r a b   c l i p b o a r d ,   s t r i p   t o t a l l y   b l a n k   l i n e s ,   t o   c h e c k   i f   t h e r e   i s   a n y t h i n g   t o   d o" &'& l �u()�u  ( 9 3   Aside: perhaps this should be on the emacs side?   ) �** f       A s i d e :   p e r h a p s   t h i s   s h o u l d   b e   o n   t h e   e m a c s   s i d e ?' +,+ l �t-.�t  - X R   for now it will stay here... could be wrong behavior, plus it is simpler to do    . �// �       f o r   n o w   i t   w i l l   s t a y   h e r e . . .   c o u l d   b e   w r o n g   b e h a v i o r ,   p l u s   i t   i s   s i m p l e r   t o   d o  , 010 l �s23�s  2       in Applescript (!)   3 �44 .           i n   A p p l e s c r i p t   ( ! )1 565 r  787 n 9:9 I  �r;�q�r "0 stripblanklines stripBlankLines; <�p< I �o�n�m
�o .JonsgClp****    ��� null�n  �m  �p  �q  :  f  8 o      �l�l 0 pasteme pasteMe6 =>= Z  <?@�k�j? =  ABA o  �i�i 0 pasteme pasteMeB m  CC �DD  @ O  "8EFE I (7�hGH
�h .sysodlogaskr        TEXTG m  (+II �JJ , N o t h i n g   t o   s e n d   S t a t a !H �gK�f
�g 
btnsK J  .3LL M�eM m  .1NN �OO  C a n c e l�e  �f  F m  "%PP�                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �k  �j  > QRQ l ==�d�c�b�d  �c  �b  R STS l ==�aUV�a  U \ V in the best of worlds, it would be possible to allow looping through the instances of   V �WW �   i n   t h e   b e s t   o f   w o r l d s ,   i t   w o u l d   b e   p o s s i b l e   t o   a l l o w   l o o p i n g   t h r o u g h   t h e   i n s t a n c e s   o fT XYX l ==�`Z[�`  Z 5 /   Stata to send the same code to each instance   [ �\\ ^       S t a t a   t o   s e n d   t h e   s a m e   c o d e   t o   e a c h   i n s t a n c eY ]^] O  =X_`_ r  AWaba l AUc�_�^c 6 AUded 2  AF�]
�] 
prcse E  ITfgf 1  JN�\
�\ 
pnamg m  OShh �ii 
 S t a t a�_  �^  b o      �[�[ 0 	thestatas 	theStatas` m  =>jj�                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  ^ klk l YY�Z�Y�X�Z  �Y  �X  l mnm r  Y`opo l Y^q�W�Vq I Y^�Ur�T
�U .corecnte****       ****r o  YZ�S�S 0 	thestatas 	theStatas�T  �W  �V  p o      �R�R 0 howmanystatas howManyStatasn sts Z  a�uv�Qwu = adxyx o  ab�P�P 0 howmanystatas howManyStatasy m  bc�O�O  v O  g�z{z k  m�|| }~} I mr�N�M�L
�N .sysobeepnull��� ��� long�M  �L  ~ �K I s��J��
�J .sysodlogaskr        TEXT� m  sv�� ��� " N o   S t a t a   r u n n i n g !� �I��H
�I 
btns� J  y~�� ��G� m  y|�� ���  C a n c e l�G  �H  �K  { m  gj���                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �Q  w k  ���� ��� Z  �����F�E� ?  ����� o  ���D�D 0 howmanystatas howManyStatas� m  ���C�C � k  ���� ��� O  ����� I ���B��
�B .sysodlogaskr        TEXT� m  ���� ��� @ n o t h i n g   f o r   m u l t i p l e   s t a t a ' s   y e t� �A��@
�A 
btns� J  ���� ��?� m  ���� ���  C a n c e l�?  �@  � m  �����                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  � ��� l ���>���>  � : 4 Stata can use the same name for different processes   � ��� h   S t a t a   c a n   u s e   t h e   s a m e   n a m e   f o r   d i f f e r e n t   p r o c e s s e s� ��� l ���=���=  � J D so... the it is impossible to cycle through Stata processes by name   � ��� �   s o . . .   t h e   i t   i s   i m p o s s i b l e   t o   c y c l e   t h r o u g h   S t a t a   p r o c e s s e s   b y   n a m e� ��<� l  ���;���;  � � � tell application "System Events"					set theStatas to (the file of every process whose name contains "Stata")				 end tell				repeat with aStata in theStatas				end repeat
				   � ���j   t e l l   a p p l i c a t i o n   " S y s t e m   E v e n t s "  	 	 	 	 	 s e t   t h e S t a t a s   t o   ( t h e   f i l e   o f   e v e r y   p r o c e s s   w h o s e   n a m e   c o n t a i n s   " S t a t a " )  	 	 	 	   e n d   t e l l  	 	 	 	 r e p e a t   w i t h   a S t a t a   i n   t h e S t a t a s  	 	 	 	 e n d   r e p e a t 
 	 	 	 	�<  �F  �E  � ��� l ���:���:  � : 4 know there is exactly one instance of Stata running   � ��� h   k n o w   t h e r e   i s   e x a c t l y   o n e   i n s t a n c e   o f   S t a t a   r u n n i n g� ��� l ���9���9  �   can finally get to work   � ��� 0   c a n   f i n a l l y   g e t   t o   w o r k� ��8� O  ����� r  ����� l ����7�6� l ����5�4� n  ����� 1  ���3
�3 
pnam� l ����2�1� n  ����� 4 ���0�
�0 
cobj� m  ���/�/ � o  ���.�. 0 	thestatas 	theStatas�2  �1  �5  �4  �7  �6  � o      �-�- 0 thestataname theStataName� m  �����                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  �8  t ��� l ���,�+�*�,  �+  �*  � ��� l ���)���)  � $  Stata *must* be made active		   � ��� <   S t a t a   * m u s t *   b e   m a d e   a c t i v e 	 	� ��� O  ���� k  ��� ��� r  ����� l ����(�'� n  ����� 1  ���&
�& 
bnid� l ����%�$� 4  ���#�
�# 
prcs� o  ���"�" 0 thestataname theStataName�%  �$  �(  �'  � o      �!�! "0 thestataversion theStataVersion� ��� r  ����� n ����� 1  ��� 
�  
txdl� 1  ���
� 
ascr� o      �� 0 theolddelims theOldDelims� ��� r  ����� J  ���� ��� m  ���� ���  c o m . s t a t a . s t a t a�  � n     ��� 1  ���
� 
txdl� 1  ���
� 
ascr� ��� r  ����� l ������ n  ����� 4 ����
� 
citm� m  ������� o  ���� "0 thestataversion theStataVersion�  �  � o      �� "0 thestataversion theStataVersion� ��� r  ����� o  ���� 0 theolddelims theOldDelims� n     ��� 1  ���
� 
txdl� 1  ���
� 
ascr� ��� r  ���� m  ���
� boovtrue� n      ��� 1  � �
� 
pisf� 4  ����
� 
prcs� l ������ o  ���� 0 thestataname theStataName�  �  �  � m  ��  �                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  �  l �
�	��
  �	  �   � Z  /� =  o  �� 0 dothis doThis m  		 �

  c o m m a n d Z  $� @   o  �� "0 thestataversion theStataVersion m  ��  n  I  �� �  0 dostatacommand doStataCommand  o  ���� 0 thestataname theStataName �� o  ���� 0 pasteme pasteMe��  �     f  �   n $ I  $������ 0 pastetmpstata pasteTmpStata  o  ���� 0 thestataname theStataName �� o   ���� 0 pasteme pasteMe��  ��    f  �   n '/ I  (/������ 0 dotmpdofile doTmpDofile   o  ()���� 0 thestataname theStataName  !"! o  )*���� 0 	tmpdofile 	tmpDoFile" #��# o  *+���� 0 dothis doThis��  ��    f  '(�   v ' !- from test of UI being turned on    w �$$ B -   f r o m   t e s t   o f   U I   b e i n g   t u r n e d   o n��    %&% l     ��������  ��  ��  & '(' i    )*) I      �������� 0 badfirstarg badFirstArg��  ��  * O     +,+ I   ��-.
�� .sysodlogaskr        TEXT- m    // �00 \ T h e   f i r s t   a r g u m e n t   m u s t   b e   " c o m m a n d "   o r   " m e n u ". ��1��
�� 
btns1 J    	22 3��3 m    44 �55  C a n c e l��  ��  , m     66�                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ( 787 l     ��������  ��  ��  8 9:9 i    ;<; I      ��=���� 0 dotmpdofile doTmpDofile= >?> o      ���� 0 	stataname 	stataName? @A@ o      ���� 0 	tmpdofile 	tmpDoFileA B��B o      ���� 0 dowhat doWhat��  ��  < k    ,CC DED l     ��FG��  F K E if multiple instances ever work, be sure this gets written just once   G �HH �   i f   m u l t i p l e   i n s t a n c e s   e v e r   w o r k ,   b e   s u r e   t h i s   g e t s   w r i t t e n   j u s t   o n c eE IJI q      KK ��L�� 
0 tmpdir  L ������ "0 stupidapplefile stupidAppleFile��  J MNM l     ��OP��  O X R need to change this, because it changes the working directory in Stata on the Mac   P �QQ �   n e e d   t o   c h a n g e   t h i s ,   b e c a u s e   i t   c h a n g e s   t h e   w o r k i n g   d i r e c t o r y   i n   S t a t a   o n   t h e   M a cN RSR r     TUT I    ��V��
�� .sysoexecTEXT���     TEXTV m     WW �XX 8 g e t c o n f   D A R W I N _ U S E R _ T E M P _ D I R��  U o      ���� 
0 tmpdir  S YZY r    [\[ l   ]����] b    ^_^ o    	���� 
0 tmpdir  _ o   	 
���� 0 	tmpdofile 	tmpDoFile��  ��  \ o      ���� 0 	tmpdofile 	tmpDoFileZ `a` l   ��bc��  b < 6 need applescript-style file name to write to the file   c �dd l   n e e d   a p p l e s c r i p t - s t y l e   f i l e   n a m e   t o   w r i t e   t o   t h e   f i l ea efe r    ghg 4    ��i
�� 
psxfi o    ���� 0 	tmpdofile 	tmpDoFileh o      ���� "0 stupidapplefile stupidAppleFilef jkj Q    glmnl k    Coo pqp I   ��rs
�� .rdwropenshor       filer o    ���� "0 stupidapplefile stupidAppleFiles ��t��
�� 
permt m    ��
�� boovtrue��  q uvu I    '��wx
�� .rdwrseofnull���     ****w o     !���� "0 stupidapplefile stupidAppleFilex ��y��
�� 
set2y m   " #����  ��  v z{z I  ( 5��|}
�� .rdwrwritnull���     ****| l  ( -~����~ I  ( -������
�� .JonsgClp****    ��� null��  ��  ��  ��  } ���
�� 
refn o   . /���� "0 stupidapplefile stupidAppleFile� �����
�� 
as  � m   0 1��
�� 
utf8��  { ��� I  6 =����
�� .rdwrwritnull���     ****� o   6 7��
�� 
ret � �����
�� 
refn� o   8 9���� "0 stupidapplefile stupidAppleFile��  � ���� I  > C�����
�� .rdwrclosnull���     ****� o   > ?���� "0 stupidapplefile stupidAppleFile��  ��  m R      ������
�� .ascrerr ****      � ****��  ��  n k   K g�� ��� I  K P�����
�� .rdwrclosnull���     ****� o   K L���� "0 stupidapplefile stupidAppleFile��  � ���� O   Q g��� I  W f����
�� .sysodlogaskr        TEXT� m   W Z�� ��� L H a d   t r o u b l e   w i t h   t h e   t e m p o r a r y   d o - f i l e� �����
�� 
btns� J   ] b�� ���� m   ] `�� ���  C a n c e l��  ��  � m   Q T���                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ��  k ��� l  h h������  � #  applescript really is a pita   � ��� :   a p p l e s c r i p t   r e a l l y   i s   a   p i t a� ��� O   h w��� I  q v������
�� .miscactvnull��� ��� null��  ��  � 4   h n���
�� 
capp� o   l m���� 0 	stataname 	stataName� ���� Z   x,������ =   x }��� o   x y���� 0 dowhat doWhat� m   y |�� ���  m e n u� Q   � ����� I   � �������� 0 	doviamenu 	doViaMenu� ���� o   � ����� 0 	stataname 	stataName��  ��  � R      ������
�� .ascrerr ****      � ****��  ��  � k   � ��� ��� I   � �������� "0 createmenuitems createMenuItems� ��� o   � ����� 0 	stataname 	stataName� ���� o   � ����� 0 	tmpdofile 	tmpDoFile��  ��  � ��� l  � �������  � 8 2 need to be sure the menu item exists on first try   � ��� d   n e e d   t o   b e   s u r e   t h e   m e n u   i t e m   e x i s t s   o n   f i r s t   t r y� ��� I  � ������
�� .sysodelanull��� ��� nmbr� m   � ����� ��  � ���� Q   � ����� I   � �������� 0 	doviamenu 	doViaMenu� ���� o   � ����� 0 	stataname 	stataName��  ��  � R      ������
�� .ascrerr ****      � ****��  ��  � O   � ���� I  � �����
�� .sysodlogaskr        TEXT� m   � ��� ��� v S o m e t h i n g   w e n t   w r o n g . . .   i s   y o u r   s t a t a   v e r s i o n   s e t   p r o p e r l y ?� �����
�� 
btns� J   � ��� ���� m   � ��� ���  C a n c e l��  ��  � m   � ����                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ��  ��  � l  �,���� Z   �,����� =   � ���� o   � ��~�~ 0 dowhat doWhat� m   � ��� ���  i n c l u d e� Q   � ����� n  � ���� I   � ��}��|�} 0 pastetmpstata pasteTmpStata� ��� o   � ��{�{ 0 	stataname 	stataName� ��z� b   � ���� m   � ��� ���  i n c l u d e  � o   � ��y�y 0 	tmpdofile 	tmpDoFile�z  �|  �  f   � �� R      �x�w�v
�x .ascrerr ****      � ****�w  �v  � O   � ���� I  � ��u��
�u .sysodlogaskr        TEXT� m   � ��� ��� \ H a d   t r o u b l e   r u n n i n g   v i a   t e m p o r a r y   i n c l u d e   f i l e� �t��s
�t 
btns� J   � ��� ��r� m   � ��� ���  C a n c e l�r  �s  � m   � ����                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �  � l  ,���� Q   ,���� k  �� ��� l �q���q  � = 7 'open' changes directory as an unavoidable side-effect   � �   n   ' o p e n '   c h a n g e s   d i r e c t o r y   a s   a n   u n a v o i d a b l e   s i d e - e f f e c t�  l �p�p     open stupidAppleFile    � *   o p e n   s t u p i d A p p l e F i l e �o n  I  �n	�m�n 0 pastetmpstata pasteTmpStata	 

 o  �l�l 0 	stataname 	stataName �k b  
 m   �  d o   o  	�j�j 0 	tmpdofile 	tmpDoFile�k  �m    f  �o  � R      �i�h�g
�i .ascrerr ****      � ****�h  �g  � O  , I +�f
�f .sysodlogaskr        TEXT m   � R H a d   t r o u b l e   r u n n i n g   v i a   t e m p o r a r y   d o - f i l e �e�d
�e 
btns J  "' �c m  "% �  C a n c e l�c  �d   m  �                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �   doing via dofile   � � "   d o i n g   v i a   d o f i l e� I C doing via dofile or include--- needs fixing for multiple instances   � � �   d o i n g   v i a   d o f i l e   o r   i n c l u d e - - -   n e e d s   f i x i n g   f o r   m u l t i p l e   i n s t a n c e s��  :   l     �b�a�`�b  �a  �`    !"! i    #$# I      �_%�^�_ 0 	doviamenu 	doViaMenu% &�]& o      �\�\ 0 	stataname 	stataName�]  �^  $ O     /'(' Z    .)*�[�Z) 1    �Y
�Y 
uien* O    *+,+ I   )�X-�W
�X .prcsclicnull��� ��� uiel- n    %./. 4   " %�V0
�V 
menI0 m   # $11 �22  r u n   t m p   f i l e/ n    "343 4    "�U5
�U 
menE5 m     !�T�T 4 n    676 4    �S8
�S 
menI8 m    99 �:: & A p p l e s c r i p t   h e l p e r s7 n    ;<; 4    �R=
�R 
menE= m    �Q�Q < n    >?> 4    �P@
�P 
mbri@ m    AA �BB  U s e r? 4    �OC
�O 
mbarC m    �N�N �W  , 4    �MD
�M 
pcapD o    �L�L 0 	stataname 	stataName�[  �Z  ( m     EE�                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  " FGF l     �K�J�I�K  �J  �I  G HIH i    JKJ I      �HL�G�H "0 createmenuitems createMenuItemsL MNM o      �F�F 0 	stataname 	stataNameN O�EO o      �D�D 0 	tmpdofile 	tmpDoFile�E  �G  K O     GPQP Z    FRS�C�BR 1    �A
�A 
uienS O    BTUT k    AVV WXW l   �@YZ�@  Y * $ get the command window to the front   Z �[[ H   g e t   t h e   c o m m a n d   w i n d o w   t o   t h e   f r o n tX \]\ I   #�?^�>
�? .prcsclicnull��� ��� uiel^ n   _`_ 4    �=a
�= 
menIa m    bb �cc  C o m m a n d` n    ded 4    �<f
�< 
menEf m    gg �hh  W i n d o we n   iji 4    �;k
�; 
mbrik m    ll �mm  W i n d o wj 4    �:n
�: 
mbarn m    �9�9 �>  ] opo I  $ +�8q�7
�8 .prcskprsnull���     ctxtq b   $ 'rsr m   $ %tt �uu r w i n d o w   m e n u   a p p e n d   s u b m e n u   " s t U s e r "   " A p p l e s c r i p t   h e l p e r s "s o   % &�6
�6 
ret �7  p vwv I  , 7�5x�4
�5 .prcskprsnull���     ctxtx b   , 3yzy b   , 1{|{ b   , /}~} m   , - ��� � w i n d o w   m e n u   a p p e n d   i t e m   " A p p l e s c r i p t   h e l p e r s "   " r u n   t m p   f i l e "   " d o  ~ o   - .�3�3 0 	tmpdofile 	tmpDoFile| m   / 0�� ���  "z o   1 2�2
�2 
ret �4  w ��1� I  8 A�0��/
�0 .prcskprsnull���     ctxt� b   8 =��� m   8 ;�� ��� & w i n d o w   m e n u   r e f r e s h� o   ; <�.
�. 
ret �/  �1  U 4    �-�
�- 
pcap� o    �,�, 0 	stataname 	stataName�C  �B  Q m     ���                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  I ��� l     �+�*�)�+  �*  �)  � ��� i    ��� I      �(��'�(  0 dostatacommand doStataCommand� ��� o      �&�& 0 	stataname 	stataName� ��%� o      �$�$ 0 
thecommand 
theCommand�%  �'  � Q     *���� w    ��� l   ���� O    ��� k    �� ��� l   �#���#  �  		activate   � ���  	 a c t i v a t e� ��"� I   �!��
�! .STscDoCanull���     ctxt� o    � �  0 
thecommand 
theCommand� ���� 0 addtoreview addToReview� m    �
� boovtrue�  �"  � 4    	��
� 
capp� o    �� 0 	stataname 	stataName�   needed to compile   � ��� $   n e e d e d   t o   c o m p i l e��                                                                                  S5x8  alis    ^  	Tucholsky                      BD ����StataMP.app                                                    ����            ����  
 cu             Stata17   </:Applications:AAApplications:MathTools:Stata17:StataMP.app/    S t a t a M P . a p p   	 T u c h o l s k y  9Applications/AAApplications/MathTools/Stata17/StataMP.app   / ��  � R      ���
� .ascrerr ****      � ****�  �  � O    *��� I    )���
� .sysodlogaskr        TEXT� m     !�� ��� l H a d   t r o u b l e   p a s s i n g   c o m m a n d ( s )   t o   S t a t a   c o m m a n d   w i n d o w� ���
� 
btns� J   " %�� ��� m   " #�� ���  C a n c e l�  �  � m    ���                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  � ��� l     ����  �  �  � ��� l     ����  �  �  � ��� i    ��� I      ���� 0 pastetmpstata pasteTmpStata� ��� o      �� 0 	stataname 	stataName� ��
� o      �	�	 0 pasteme pasteMe�
  �  � k     ��� ��� q      �� ��� 0 oldclipboard oldClipBoard�  � ��� r     ��� I    ���
� .JonsgClp****    ��� null�  �  � o      �� 0 oldclipboard oldClipBoard� ��� Q    ����� k    h�� ��� I   ���
� .JonspClpnull���     ****� l   �� ��� c    ��� o    ���� 0 pasteme pasteMe� m    ��
�� 
ctxt�   ��  �  � ���� O    h��� Z    g������� 1    ��
�� 
uien� O    c��� k   % b�� ��� I  % 6�����
�� .prcsclicnull��� ��� uiel� n  % 2��� 4   / 2���
�� 
menI� m   0 1�� ���  C o m m a n d� n   % /��� 4   , /���
�� 
menE� m   - .�� ���  W i n d o w� n  % ,��� 4   ) ,���
�� 
mbri� m   * +�� ���  W i n d o w� 4   % )���
�� 
mbar� m   ' (���� ��  � ��� I  7 <�����
�� .sysodelanull��� ��� nmbr� m   7 8�� ?ə�������  � ��� I  = T�����
�� .prcsclicnull��� ��� uiel� n  = P��� 4   K P���
�� 
menI� l 	 L O������ m   L O�� ��� 
 P a s t e��  ��  � n   = K� � 4   F K��
�� 
menE m   G J �  E d i t  n  = F 4   A F��
�� 
mbri m   B E �  E d i t 4   = A��	
�� 
mbar	 m   ? @���� ��  � 

 l  U U����   > 8 added delay when seeing odd behavior on machine at work    � p   a d d e d   d e l a y   w h e n   s e e i n g   o d d   b e h a v i o r   o n   m a c h i n e   a t   w o r k  l  U U����   U O it seems that a delay of under 0.2 seconds makes things weird on fast machines    � �   i t   s e e m s   t h a t   a   d e l a y   o f   u n d e r   0 . 2   s e c o n d s   m a k e s   t h i n g s   w e i r d   o n   f a s t   m a c h i n e s  I  U Z����
�� .sysodelanull��� ��� nmbr m   U V ?ə�������   �� I  [ b����
�� .prcskprsnull���     ctxt o   [ ^��
�� 
ret ��  ��  � 4    "��
�� 
pcap o     !���� 0 	stataname 	stataName��  ��  � m    �                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  ��  � R      ������
�� .ascrerr ****      � ****��  ��  � k   p �  I  p u����
�� .JonspClpnull���     **** o   p q���� 0 oldclipboard oldClipBoard��    ��  O   v �!"! I  | ���#$
�� .sysodlogaskr        TEXT# m   | %% �&& V H a d   t r o u b l e   p a s t i n g   t o   S t a t a   c o m m a n d   w i n d o w$ ��'��
�� 
btns' J   � �(( )��) m   � �** �++  C a n c e l��  ��  " m   v y,,�                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ��  � -��- I  � ���.��
�� .JonspClpnull���     ****. o   � ����� 0 oldclipboard oldClipBoard��  ��  � /0/ l     ��������  ��  ��  0 121 l     ��������  ��  ��  2 343 i    565 I      ��7���� "0 stripblanklines stripBlankLines7 8��8 o      ���� 0 thetext theText��  ��  6 k     i99 :;: q      << ��=�� 0 theparas theParas= ��>�� 0 thepara thePara> ��?�� 0 achar aChar? ��@�� 0 thecleanstuff theCleanStuff@ ������ 0 eraseme  ��  ; ABA r     CDC l    E����E n     FGF 2   ��
�� 
cparG o     ���� 0 thetext theText��  ��  D o      ���� 0 theparas theParasB HIH r    
JKJ J    ����  K o      ���� 0 thecleanstuff theCleanStuffI LML X    ^N��ON k    YPP QRQ r    STS m    ��
�� boovtrueT o      ���� 0 eraseme  R UVU X    IW��XW Z   1 DYZ����Y H   1 8[[ E  1 7\]\ J   1 5^^ _`_ m   1 2aa �bb   ` c��c m   2 3dd �ee  	��  ] o   5 6���� 0 achar aCharZ k   ; @ff ghg r   ; >iji m   ; <��
�� boovfalsj o      ���� 0 eraseme  h k��k  S   ? @��  ��  ��  �� 0 achar aCharX n   " %lml 2   # %��
�� 
cha m o   " #���� 0 thepara theParaV n��n Z   J Yop����o H   J Lqq o   J K���� 0 eraseme  p r   O Ursr l  O Rt����t c   O Ruvu o   O P���� 0 thepara theParav m   P Q��
�� 
TEXT��  ��  s l     w����w n      xyx  ;   S Ty o   R S���� 0 thecleanstuff theCleanStuff��  ��  ��  ��  ��  �� 0 thepara theParaO o    ���� 0 theparas theParasM z{z r   _ d|}| o   _ `��
�� 
ret } l     ~����~ 1   ` c��
�� 
txdl��  ��  { �� L   e i�� l  e h������ c   e h��� o   e f���� 0 thecleanstuff theCleanStuff� m   f g��
�� 
TEXT��  ��  ��  4 ��� l     ��������  ��  ��  � ���� i     #��� I      �������� 0 getosxversion getOSXversion��  ��  � l    H���� k     H�� ��� q      �� ������ 0 oslist osList��  � ��� r     ��� m     �� ���  .� l     ������ 1    ��
�� 
txdl��  ��  � ��� O    ��� r   
 ��� l  
 ������ 1   
 ��
�� 
vers��  ��  � o      ���� 0 oslist osList� m    ���                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  � ��� r    ��� n    ��� 2    ��
�� 
citm� o    ���� 0 oslist osList� o      ���� 0 oslist osList� ��� Y    4�������� r   $ /��� l  $ *���~� c   $ *��� n   $ (��� 4   % (�}�
�} 
cobj� o   & '�|�| 0 cnt  � o   $ %�{�{ 0 oslist osList� m   ( )�z
�z 
long�  �~  � n      ��� 4   + .�y�
�y 
cobj� o   , -�x�x 0 cnt  � o   * +�w�w 0 oslist osList�� 0 cnt  � m    �v�v � l   ��u�t� n    ��� 1    �s
�s 
leng� o    �r�r 0 oslist osList�u  �t  ��  � ��� Z   5 E���q�p� =  5 :��� l  5 8��o�n� n   5 8��� 1   6 8�m
�m 
leng� o   5 6�l�l 0 oslist osList�o  �n  � m   8 9�k�k � s   = A��� m   = >�j�j  � l     ��i�h� n      ���  ;   ? @� o   > ?�g�g 0 oslist osList�i  �h  �q  �p  � ��f� L   F H�� o   F G�e�e 0 oslist osList�f  � 3 - returns a list with major and minor versions   � ��� Z   r e t u r n s   a   l i s t   w i t h   m a j o r   a n d   m i n o r   v e r s i o n s��       �d�����������d  � 	�c�b�a�`�_�^�]�\�[
�c .aevtoappnull  �   � ****�b 0 badfirstarg badFirstArg�a 0 dotmpdofile doTmpDofile�` 0 	doviamenu 	doViaMenu�_ "0 createmenuitems createMenuItems�^  0 dostatacommand doStataCommand�] 0 pastetmpstata pasteTmpStata�\ "0 stripblanklines stripBlankLines�[ 0 getosxversion getOSXversion� �Z !�Y�X���W
�Z .aevtoappnull  �   � ****�Y 0 args  �X  � �V�U�T�S�R�Q�P�O�N�M�L�K�J�I�H�G�V 0 args  �U 0 numargs numArgs�T 0 pasteme pasteMe�S 0 dothis doThis�R 0 	tmpdofile 	tmpDoFile�Q 0 howmanystatas howManyStatas�P 0 	thestatas 	theStatas�O 0 thestataname theStataName�N "0 thestataversion theStataVersion�M 0 theolddelims theOldDelims�L &0 thesplitosversion theSplitOSVersion�K 0 osmajor osMajor�J 0 osminor osMinor�I $0 defaulttmpdofile defaultTmpDoFile�H 0 uiok UIOK�G 0 errmsg errMsg� = G _�F�E�D�C�B ��A�@�?�>�=�< ��; ��: ��9 � � � � ��8 � � � ��7�6�5�4�3CPIN�2��1h�0�/�����.�-�,��+�*	�)�(�'�&
�F 
uien�E 0 errmsg errMsg�D  �C 0 getosxversion getOSXversion
�B 
cobj
�A .miscactvnull��� ��� null�@ 
�? 0 opminor opMinor�> 	
�= 
bool
�< 
xppb
�; 
xpcp
�: 
btns
�9 .sysodlogaskr        TEXT
�8 
leng�7 �6 0 badfirstarg badFirstArg�5  
�4 .JonsgClp****    ��� null�3 "0 stripblanklines stripBlankLines
�2 
prcs�  
�1 
pnam
�0 .corecnte****       ****
�/ .sysobeepnull��� ��� long
�. 
bnid
�- 
ascr
�, 
txdl
�+ 
citm
�* 
pisf�) �(  0 dostatacommand doStataCommand�' 0 pastetmpstata pasteTmpStata�& 0 dotmpdofile doTmpDofile�W0�E�O � *�,E eE�Y fE�UW 
X  fE�O� �)j+ E[�k/E�Z[�l/E�ZO� c*j O��	 ���& *��/*�,FOa a a kvl Y 4*�a /*�,FO�� a a a kvl Y a a a kvl UY��a ,E�O N��k/E�Oa a a a a v� 
)j+ Y hO�k ��l/E�O�a    �E�Y hY �E�W X ! )j+ O)*j "k+ #E�O�a $  a % a &a a 'kvl UY hO� *a (-a )[a *,\Za +@1E�UO�j ,E�O�j  !a % *j -Oa .a a /kvl UY 4�k a % a 0a a 1kvl UOPY hO� ��k/a *,E�UO� F*a (�/a 2,E�O_ 3a 4,E�Oa 5kv_ 3a 4,FO�a 6i/E�O�_ 3a 4,FOe*a (�/a 7,FUO�a 8  �a 9 )��l+ :Y 	)��l+ ;Y 
)���m+ <� �%*�$�#���"�% 0 badfirstarg badFirstArg�$  �#  �  � 6/�!4� 
�! 
btns
�  .sysodlogaskr        TEXT�" � ���kvl U� �<������ 0 dotmpdofile doTmpDofile� ��� �  ���� 0 	stataname 	stataName� 0 	tmpdofile 	tmpDoFile� 0 dowhat doWhat�  � ������ 0 	stataname 	stataName� 0 	tmpdofile 	tmpDoFile� 0 dowhat doWhat� 
0 tmpdir  � "0 stupidapplefile stupidAppleFile� &W���������
�	������������ �����������������
� .sysoexecTEXT���     TEXT
� 
psxf
� 
perm
� .rdwropenshor       file
� 
set2
� .rdwrseofnull���     ****
� .JonsgClp****    ��� null
� 
refn
�
 
as  
�	 
utf8� 
� .rdwrwritnull���     ****
� 
ret 
� .rdwrclosnull���     ****�  �  
� 
btns
� .sysodlogaskr        TEXT
�  
capp
�� .miscactvnull��� ��� null�� 0 	doviamenu 	doViaMenu�� "0 createmenuitems createMenuItems
�� .sysodelanull��� ��� nmbr�� 0 pastetmpstata pasteTmpStata�-�j E�O��%E�O*�/E�O 0��el O��jl O*j ���� O��l O�j W #X  �j Oa  a a a kvl UO*a �/ *j UO�a   K *�k+ W <X  *��l+ Okj O *�k+ W X  a  a a a kvl UY e�a   1 )�a �%l+  W X  a  a !a a "kvl UY . )�a #�%l+  W X  a  a $a a %kvl U� ��$���������� 0 	doviamenu 	doViaMenu�� ����� �  ���� 0 	stataname 	stataName��  � ���� 0 	stataname 	stataName� E��������A����91��
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
UY hU� ��K���������� "0 createmenuitems createMenuItems�� ����� �  ������ 0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFile��  � ������ 0 	stataname 	stataName�� 0 	tmpdofile 	tmpDoFile� ���������l��g��b��t������
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
O��%j O�%�%�%j Oa �%j UY hU� �������������  0 dostatacommand doStataCommand�� ����� �  ������ 0 	stataname 	stataName�� 0 
thecommand 
theCommand��  � ������ 0 	stataname 	stataName�� 0 
thecommand 
theCommand� ������������������
�� 
capp�� 0 addtoreview addToReview
�� .STscDoCanull���     ctxt��  ��  
�� 
btns
�� .sysodlogaskr        TEXT�� + �Z*�/ 	��el UW X  � ���kvl 
U� ������������� 0 pastetmpstata pasteTmpStata�� ����� �  ������ 0 	stataname 	stataName�� 0 pasteme pasteMe��  � �������� 0 	stataname 	stataName�� 0 pasteme pasteMe�� 0 oldclipboard oldClipBoard� �����������������������������������,%��*��
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
�� .sysodlogaskr        TEXT�� �*j  E�O b��&j O� R*�,E J*�/ ?*�k/��/��/��/j O�j O*�k/�a /�a /�a /j O�j O_ j UY hUW #X  �j Oa  a a a kvl UO�j � ��6���������� "0 stripblanklines stripBlankLines�� ����� �  ���� 0 thetext theText��  � �������������� 0 thetext theText�� 0 theparas theParas�� 0 thepara thePara�� 0 achar aChar�� 0 thecleanstuff theCleanStuff�� 0 eraseme  � 
����������ad������
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
fE�OY h[OY��O� ��&�6FY h[OY��O�*�,FO��&� ������������� 0 getosxversion getOSXversion��  ��  � ������ 0 oslist osList�� 0 cnt  � ��������������
�� 
txdl
�� 
vers
�� 
citm
�� 
leng
�� 
cobj
�� 
long�� I�*�,FO� *�,E�UO��-E�O k��,Ekh ��/�&��/F[OY��O��,k  	j�6GY hO�ascr  ��ޭ