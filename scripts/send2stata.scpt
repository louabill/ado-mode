FasdUAS 1.101.10   ��   ��    k             l      ��  ��    ( " version 2.1.3 -  August 29, 2016      � 	 	 D   v e r s i o n   2 . 1 . 3   -     A u g u s t   2 9 ,   2 0 1 6     
  
 l      ��  ��    , & sends contents of clipboard to Stata      �   L   s e n d s   c o n t e n t s   o f   c l i p b o a r d   t o   S t a t a        l      ��  ��    U O allows running from command window, as a temporary do-file or via a menu item      �   �   a l l o w s   r u n n i n g   f r o m   c o m m a n d   w i n d o w ,   a s   a   t e m p o r a r y   d o - f i l e   o r   v i a   a   m e n u   i t e m        l      ��  ��    N H applescript bug: single bar in front of "include" causes compile error      �   �   a p p l e s c r i p t   b u g :   s i n g l e   b a r   i n   f r o n t   o f   " i n c l u d e "   c a u s e s   c o m p i l e   e r r o r        l      ��  ��    W Q args are: { "command" | "menu" | "dofile" || "include" } [ name-of-tmp-dofile ]      �   �   a r g s   a r e :   {   " c o m m a n d "   |   " m e n u "   |   " d o f i l e "   | |   " i n c l u d e "   }   [   n a m e - o f - t m p - d o f i l e   ]        i        !   I     �� "��
�� .aevtoappnull  �   � **** " o      ���� 0 args  ��   ! k     # #  $ % $ l     �� & '��   &  - initializations    ' � ( ( " -   i n i t i a l i z a t i o n s %  ) * ) l     �� + ,��   + � �- NOTE: all vars using in the run handler MUST be local to keep this file from resaving itself ad-nauseum, messing up the git repo    , � - - -   N O T E :   a l l   v a r s   u s i n g   i n   t h e   r u n   h a n d l e r   M U S T   b e   l o c a l   t o   k e e p   t h i s   f i l e   f r o m   r e s a v i n g   i t s e l f   a d - n a u s e u m ,   m e s s i n g   u p   t h e   g i t   r e p o *  . / . q       0 0 �� 1�� 0 numargs numArgs 1 �� 2�� 0 pasteme pasteMe 2 �� 3�� 0 dothis doThis 3 ������ 0 	tmpdofile 	tmpDoFile��   /  4 5 4 q       6 6 �� 7�� 0 howmanystatas howManyStatas 7 �� 8�� 0 	thestatas 	theStatas 8 �� 9�� 0 thestataname theStataName 9 ������ "0 thestataversion theStataVersion��   5  : ; : q       < < �� =�� 0 theolddelims theOldDelims = ������ 0 theosxversion theOSXVersion��   ;  > ? > q       @ @ ������ $0 defaulttmpdofile defaultTmpDoFile��   ?  A B A r      C D C m      E E � F F  f e e d S t a t a . d o D o      ���� $0 defaulttmpdofile defaultTmpDoFile B  G H G q     I I ������ 0 uiok UIOK��   H  J K J l   �� L M��   L / ) first check that UI scripting will work	    M � N N R   f i r s t   c h e c k   t h a t   U I   s c r i p t i n g   w i l l   w o r k 	 K  O P O O     Q R Q Z     S T�� U S 1    ��
�� 
uien T r     V W V m    ��
�� boovtrue W o      ���� 0 uiok UIOK��   U r     X Y X m    ��
�� boovfals Y o      ���� 0 uiok UIOK R m     Z Z�                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��   P  [ \ [ l   �� ] ^��   ] G A macOS 11 seems too have messed with the UI elements enabled item    ^ � _ _ �   m a c O S   1 1   s e e m s   t o o   h a v e   m e s s e d   w i t h   t h e   U I   e l e m e n t s   e n a b l e d   i t e m \  ` a ` l   �� b c��   b E ? I'm not sure anymore how to check to see if things can be sent    c � d d ~   I ' m   n o t   s u r e   a n y m o r e   h o w   t o   c h e c k   t o   s e e   i f   t h i n g s   c a n   b e   s e n t a  e f e O    0 g h g Z    / i j���� i ?    % k l k l   # m���� m c    # n o n 1    !��
�� 
vers o m   ! "��
�� 
nmbr��  ��   l m   # $���� 
 j r   ( + p q p m   ( )��
�� boovtrue q o      ���� 0 uiok UIOK��  ��   h m     r r�                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��   f  s�� s l  1 t u v t Z   1 w x�� y w H   1 3 z z o   1 2���� 0 uiok UIOK x O   6 � { | { k   : � } }  ~  ~ r   : C � � � c   : A � � � l  : ? ����� � n  : ? � � � I   ; ?�������� 0 getosxversion getOSXversion��  ��   �  f   : ;��  ��   � m   ? @��
�� 
long � o      ���� 0 theosxversion theOSXVersion   ��� � Z   D � � ��� � � B   D G � � � o   D E���� 0 theosxversion theOSXVersion � m   E F���� 	 � k   J f � �  � � � I  J O������
�� .miscactvnull��� ��� null��  ��   �  � � � r   P X � � � 4   P T�� �
�� 
xppb � m   R S � � � � � H c o m . a p p l e . p r e f e r e n c e . u n i v e r s a l a c c e s s � 1   T W��
�� 
xpcp �  ��� � I  Y f�� � �
�� .sysodlogaskr        TEXT � m   Y Z � � � � � � W h e n   S y s t e m   P r e f e r e n c e s   o p e n s ,   b e   s u r e   t h a t   ' E n a b l e   a c c e s s   f o r   a s s i s t i v e   d e v i c e s '   i s   c h e c k e d ,   t h e n   t r y   a g a i n . � �� ���
�� 
btns � J   ] b � �  ��� � m   ] ` � � � � �  O K��  ��  ��  ��   � k   i � � �  � � � I  i n������
�� .miscactvnull��� ��� null��  ��   �  � � � r   o y � � � 4   o u�� �
�� 
xppb � m   q t � � � � � : c o m . a p p l e . p r e f e r e n c e . s e c u r i t y � 1   u x��
�� 
xpcp �  ��� � I  z ��� � �
�� .sysodlogaskr        TEXT � m   z } � � � � �j W h e n   t h e   S e c u r i t y   &   P r i v a c y   p r e f e r e n c e   p a n e   o p e n s ,   s e l e c t   t h e   P r i v a c y   t a b ,   t h e n   s e l e c t   t h e   A c c e s s i b i l i t y   i t e m   a n d   b e   s u r e   y o u r   v e r s i o n   o f   E m a c s   i s   c h e c k e d .   W h e n   f i n i s h e d ,   t r y   a g a i n . � �� ���
�� 
btns � J   � � � �  ��� � m   � � � � � � �  O K��  ��  ��  ��   | m   6 7 � ��                                                                                  sprf  alis    Z  	Tucholsky                      BD ����System Preferences.app                                         ����            ����  
 cu             Applications  -/:System:Applications:System Preferences.app/   .  S y s t e m   P r e f e r e n c e s . a p p   	 T u c h o l s k y  *System/Applications/System Preferences.app  / ��  ��   y k   � � �  � � � l  � ��� � ���   � ' ! check proper number of arguments    � � � � B   c h e c k   p r o p e r   n u m b e r   o f   a r g u m e n t s �  � � � r   � � � � � l  � � ����� � n   � � � � � 1   � ���
�� 
leng � o   � ����� 0 args  ��  ��   � o      ���� 0 numargs numArgs �  � � � Q   � � � � � � k   � � � �  � � � r   � � � � � n   � � � � � 4   � ��� �
�� 
cobj � m   � �����  � o   � ����� 0 args   � o      ���� 0 dothis doThis �  � � � Z   � � � ����� � H   � � � � E   � � � � � J   � � � �  � � � m   � � � � � � �  c o m m a n d �  � � � m   � � � � � � �  m e n u �  � � � m   � � � � � � �  d o f i l e �  ��� � m   � � � � � � �  i n c l u d e��   � o   � ����� 0 dothis doThis � n  � � � � � I   � ��������� 0 badfirstarg badFirstArg��  ��   �  f   � ���  ��   �  ��� � Z   � � � ��� � � ?   � � � � � o   � ����� 0 numargs numArgs � m   � �����  � k   � � � �  � � � r   � � � � � n   � � � � � 4   � ��� �
�� 
cobj � m   � �����  � o   � ����� 0 args   � o      ���� 0 	tmpdofile 	tmpDoFile �  ��� � Z   � � � ����� � =   � � � � � o   � ����� 0 	tmpdofile 	tmpDoFile � m   � � � � � � �   � r   � � � � � o   � ����� $0 defaulttmpdofile defaultTmpDoFile � o      ���� 0 	tmpdofile 	tmpDoFile��  ��  ��  ��   � r   � �   o   � ����� $0 defaulttmpdofile defaultTmpDoFile o      ���� 0 	tmpdofile 	tmpDoFile��   � R      ������
�� .ascrerr ****      � ****��  ��   � l  � � n  � � I   � ��������� 0 badfirstarg badFirstArg��  ��    f   � �   no arguments    �    n o   a r g u m e n t s � 	 l  � ���������  ��  ��  	 

 l  � �����   U O grab clipboard, strip totally blank lines, to check if there is anything to do    � �   g r a b   c l i p b o a r d ,   s t r i p   t o t a l l y   b l a n k   l i n e s ,   t o   c h e c k   i f   t h e r e   i s   a n y t h i n g   t o   d o  l  � �����   9 3   Aside: perhaps this should be on the emacs side?    � f       A s i d e :   p e r h a p s   t h i s   s h o u l d   b e   o n   t h e   e m a c s   s i d e ?  l  � �����   X R   for now it will stay here... could be wrong behavior, plus it is simpler to do     � �       f o r   n o w   i t   w i l l   s t a y   h e r e . . .   c o u l d   b e   w r o n g   b e h a v i o r ,   p l u s   i t   i s   s i m p l e r   t o   d o    l  � �����         in Applescript (!)    � .           i n   A p p l e s c r i p t   ( ! )  r   � � !  n  � �"#" I   � ���$��� "0 stripblanklines stripBlankLines$ %�~% I  � ��}�|�{
�} .JonsgClp****    ��� null�|  �{  �~  �  #  f   � �! o      �z�z 0 pasteme pasteMe &'& Z    ()�y�x( =   *+* o   �w�w 0 pasteme pasteMe+ m  ,, �--  ) O  ./. I �v01
�v .sysodlogaskr        TEXT0 m  22 �33 , N o t h i n g   t o   s e n d   S t a t a !1 �u4�t
�u 
btns4 J  55 6�s6 m  77 �88  C a n c e l�s  �t  / m  	99�                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �y  �x  ' :;: l !!�r�q�p�r  �q  �p  ; <=< l !!�o>?�o  > \ V in the best of worlds, it would be possible to allow looping through the instances of   ? �@@ �   i n   t h e   b e s t   o f   w o r l d s ,   i t   w o u l d   b e   p o s s i b l e   t o   a l l o w   l o o p i n g   t h r o u g h   t h e   i n s t a n c e s   o f= ABA l !!�nCD�n  C 5 /   Stata to send the same code to each instance   D �EE ^       S t a t a   t o   s e n d   t h e   s a m e   c o d e   t o   e a c h   i n s t a n c eB FGF O  !<HIH r  %;JKJ l %9L�m�lL 6 %9MNM 2  %*�k
�k 
prcsN E  -8OPO 1  .2�j
�j 
pnamP m  37QQ �RR 
 S t a t a�m  �l  K o      �i�i 0 	thestatas 	theStatasI m  !"SS�                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  G TUT l ==�h�g�f�h  �g  �f  U VWV r  =DXYX l =BZ�e�dZ I =B�c[�b
�c .corecnte****       ****[ o  =>�a�a 0 	thestatas 	theStatas�b  �e  �d  Y o      �`�` 0 howmanystatas howManyStatasW \]\ Z  E�^_�_`^ = EHaba o  EF�^�^ 0 howmanystatas howManyStatasb m  FG�]�]  _ O  Kecdc k  Odee fgf I OT�\�[�Z
�\ .sysobeepnull��� ��� long�[  �Z  g h�Yh I Ud�Xij
�X .sysodlogaskr        TEXTi m  UXkk �ll " N o   S t a t a   r u n n i n g !j �Wm�V
�W 
btnsm J  [`nn o�Uo m  [^pp �qq  C a n c e l�U  �V  �Y  d m  KLrr�                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �_  ` k  h�ss tut Z  h�vw�T�Sv ?  hkxyx o  hi�R�R 0 howmanystatas howManyStatasy m  ij�Q�Q w k  n�zz {|{ O  n�}~} I r��P�
�P .sysodlogaskr        TEXT m  ru�� ��� @ n o t h i n g   f o r   m u l t i p l e   s t a t a ' s   y e t� �O��N
�O 
btns� J  x}�� ��M� m  x{�� ���  C a n c e l�M  �N  ~ m  no���                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  | ��� l ���L���L  � : 4 Stata can use the same name for different processes   � ��� h   S t a t a   c a n   u s e   t h e   s a m e   n a m e   f o r   d i f f e r e n t   p r o c e s s e s� ��� l ���K���K  � J D so... the it is impossible to cycle through Stata processes by name   � ��� �   s o . . .   t h e   i t   i s   i m p o s s i b l e   t o   c y c l e   t h r o u g h   S t a t a   p r o c e s s e s   b y   n a m e� ��J� l  ���I���I  � � � tell application "System Events"					set theStatas to (the file of every process whose name contains "Stata")				 end tell				repeat with aStata in theStatas				end repeat
				   � ���j   t e l l   a p p l i c a t i o n   " S y s t e m   E v e n t s "  	 	 	 	 	 s e t   t h e S t a t a s   t o   ( t h e   f i l e   o f   e v e r y   p r o c e s s   w h o s e   n a m e   c o n t a i n s   " S t a t a " )  	 	 	 	   e n d   t e l l  	 	 	 	 r e p e a t   w i t h   a S t a t a   i n   t h e S t a t a s  	 	 	 	 e n d   r e p e a t 
 	 	 	 	�J  �T  �S  u ��� l ���H���H  � : 4 know there is exactly one instance of Stata running   � ��� h   k n o w   t h e r e   i s   e x a c t l y   o n e   i n s t a n c e   o f   S t a t a   r u n n i n g� ��� l ���G���G  �   can finally get to work   � ��� 0   c a n   f i n a l l y   g e t   t o   w o r k� ��F� O  ����� r  ����� l ����E�D� l ����C�B� n  ����� 1  ���A
�A 
pnam� l ����@�?� n  ����� 4 ���>�
�> 
cobj� m  ���=�= � o  ���<�< 0 	thestatas 	theStatas�@  �?  �C  �B  �E  �D  � o      �;�; 0 thestataname theStataName� m  �����                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  �F  ] ��� l ���:�9�8�:  �9  �8  � ��� l ���7���7  � $  Stata *must* be made active		   � ��� <   S t a t a   * m u s t *   b e   m a d e   a c t i v e 	 	� ��� O  ����� k  ���� ��� r  ����� l ����6�5� n  ����� 1  ���4
�4 
bnid� l ����3�2� 4  ���1�
�1 
prcs� o  ���0�0 0 thestataname theStataName�3  �2  �6  �5  � o      �/�/ "0 thestataversion theStataVersion� ��� r  ����� n ����� 1  ���.
�. 
txdl� 1  ���-
�- 
ascr� o      �,�, 0 theolddelims theOldDelims� ��� r  ����� J  ���� ��+� m  ���� ���  c o m . s t a t a . s t a t a�+  � n     ��� 1  ���*
�* 
txdl� 1  ���)
�) 
ascr� ��� r  ����� l ����(�'� n  ����� 4 ���&�
�& 
citm� m  ���%�%��� o  ���$�$ "0 thestataversion theStataVersion�(  �'  � o      �#�# "0 thestataversion theStataVersion� ��� r  ����� o  ���"�" 0 theolddelims theOldDelims� n     ��� 1  ���!
�! 
txdl� 1  ��� 
�  
ascr� ��� r  ����� m  ���
� boovtrue� n      ��� 1  ���
� 
pisf� 4  ����
� 
prcs� l ������ o  ���� 0 thestataname theStataName�  �  �  � m  �����                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  � ��� l ������  �  �  � ��� Z  ������ = ����� o  ���� 0 dothis doThis� m  ���� ���  c o m m a n d� Z  ������ @  ����� o  ���� "0 thestataversion theStataVersion� m  ���� � n ����� I  ������  0 dostatacommand doStataCommand� ��� o  ���� 0 thestataname theStataName� ��� o  ���� 0 pasteme pasteMe�  �  �  f  ���  � n �� � I   �
�	�
 0 pastetmpstata pasteTmpStata  o   �� 0 thestataname theStataName � o  �� 0 pasteme pasteMe�  �	     f  � �  � n 	 I  
��� 0 dotmpdofile doTmpDofile 	 o  
�� 0 thestataname theStataName	 

 o  �� 0 	tmpdofile 	tmpDoFile � o  � �  0 dothis doThis�  �    f  	
�   u ' !- from test of UI being turned on    v � B -   f r o m   t e s t   o f   U I   b e i n g   t u r n e d   o n��     l     ��������  ��  ��    i     I      �������� 0 badfirstarg badFirstArg��  ��   O      I   ��
�� .sysodlogaskr        TEXT m     � \ T h e   f i r s t   a r g u m e n t   m u s t   b e   " c o m m a n d "   o r   " m e n u " ����
�� 
btns J    	 �� m     �  C a n c e l��  ��   m     �                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��    !  l     ��������  ��  ��  ! "#" i    $%$ I      ��&���� 0 dotmpdofile doTmpDofile& '(' o      ���� 0 	stataname 	stataName( )*) o      ���� 0 	tmpdofile 	tmpDoFile* +��+ o      ���� 0 dowhat doWhat��  ��  % k    ,,, -.- l     ��/0��  / K E if multiple instances ever work, be sure this gets written just once   0 �11 �   i f   m u l t i p l e   i n s t a n c e s   e v e r   w o r k ,   b e   s u r e   t h i s   g e t s   w r i t t e n   j u s t   o n c e. 232 q      44 ��5�� 
0 tmpdir  5 ������ "0 stupidapplefile stupidAppleFile��  3 676 l     ��89��  8 X R need to change this, because it changes the working directory in Stata on the Mac   9 �:: �   n e e d   t o   c h a n g e   t h i s ,   b e c a u s e   i t   c h a n g e s   t h e   w o r k i n g   d i r e c t o r y   i n   S t a t a   o n   t h e   M a c7 ;<; r     =>= I    ��?��
�� .sysoexecTEXT���     TEXT? m     @@ �AA 8 g e t c o n f   D A R W I N _ U S E R _ T E M P _ D I R��  > o      ���� 
0 tmpdir  < BCB r    DED l   F����F b    GHG o    	���� 
0 tmpdir  H o   	 
���� 0 	tmpdofile 	tmpDoFile��  ��  E o      ���� 0 	tmpdofile 	tmpDoFileC IJI l   ��KL��  K < 6 need applescript-style file name to write to the file   L �MM l   n e e d   a p p l e s c r i p t - s t y l e   f i l e   n a m e   t o   w r i t e   t o   t h e   f i l eJ NON r    PQP 4    ��R
�� 
psxfR o    ���� 0 	tmpdofile 	tmpDoFileQ o      ���� "0 stupidapplefile stupidAppleFileO STS Q    gUVWU k    CXX YZY I   ��[\
�� .rdwropenshor       file[ o    ���� "0 stupidapplefile stupidAppleFile\ ��]��
�� 
perm] m    ��
�� boovtrue��  Z ^_^ I    '��`a
�� .rdwrseofnull���     ****` o     !���� "0 stupidapplefile stupidAppleFilea ��b��
�� 
set2b m   " #����  ��  _ cdc I  ( 5��ef
�� .rdwrwritnull���     ****e l  ( -g����g I  ( -������
�� .JonsgClp****    ��� null��  ��  ��  ��  f ��hi
�� 
refnh o   . /���� "0 stupidapplefile stupidAppleFilei ��j��
�� 
as  j m   0 1��
�� 
utf8��  d klk I  6 =��mn
�� .rdwrwritnull���     ****m o   6 7��
�� 
ret n ��o��
�� 
refno o   8 9���� "0 stupidapplefile stupidAppleFile��  l p��p I  > C��q��
�� .rdwrclosnull���     ****q o   > ?���� "0 stupidapplefile stupidAppleFile��  ��  V R      ������
�� .ascrerr ****      � ****��  ��  W k   K grr sts I  K P��u��
�� .rdwrclosnull���     ****u o   K L���� "0 stupidapplefile stupidAppleFile��  t v��v O   Q gwxw I  W f��yz
�� .sysodlogaskr        TEXTy m   W Z{{ �|| L H a d   t r o u b l e   w i t h   t h e   t e m p o r a r y   d o - f i l ez ��}��
�� 
btns} J   ] b~~ �� m   ] `�� ���  C a n c e l��  ��  x m   Q T���                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ��  T ��� l  h h������  � #  applescript really is a pita   � ��� :   a p p l e s c r i p t   r e a l l y   i s   a   p i t a� ��� O   h w��� I  q v������
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
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ��  ��  � l  �,���� Z   �,������ =   � ���� o   � ����� 0 dowhat doWhat� m   � ��� ���  i n c l u d e� Q   � ����� n  � ���� I   � �������� 0 pastetmpstata pasteTmpStata� ��� o   � ����� 0 	stataname 	stataName� ���� b   � ���� m   � ��� ���  i n c l u d e  � o   � ����� 0 	tmpdofile 	tmpDoFile��  ��  �  f   � �� R      ������
�� .ascrerr ****      � ****��  ��  � O   � ���� I  � �����
�� .sysodlogaskr        TEXT� m   � ��� ��� \ H a d   t r o u b l e   r u n n i n g   v i a   t e m p o r a r y   i n c l u d e   f i l e� �����
�� 
btns� J   � ��� ���� m   � ��� ���  C a n c e l��  ��  � m   � ����                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ��  � l  ,���� Q   ,���� k  �� ��� l ����  � = 7 'open' changes directory as an unavoidable side-effect   � ��� n   ' o p e n '   c h a n g e s   d i r e c t o r y   a s   a n   u n a v o i d a b l e   s i d e - e f f e c t� ��� l �~���~  �   open stupidAppleFile   � ��� *   o p e n   s t u p i d A p p l e F i l e� ��}� n ��� I  �|��{�| 0 pastetmpstata pasteTmpStata� ��� o  �z�z 0 	stataname 	stataName� ��y� b  
��� m  �� ���  d o  � o  	�x�x 0 	tmpdofile 	tmpDoFile�y  �{  �  f  �}  � R      �w�v�u
�w .ascrerr ****      � ****�v  �u  � O  ,��� I +�t��
�t .sysodlogaskr        TEXT� m  �� ��� R H a d   t r o u b l e   r u n n i n g   v i a   t e m p o r a r y   d o - f i l e� �s �r
�s 
btns  J  "' �q m  "% �  C a n c e l�q  �r  � m  �                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  �   doing via dofile   � � "   d o i n g   v i a   d o f i l e� I C doing via dofile or include--- needs fixing for multiple instances   � � �   d o i n g   v i a   d o f i l e   o r   i n c l u d e - - -   n e e d s   f i x i n g   f o r   m u l t i p l e   i n s t a n c e s��  # 	 l     �p�o�n�p  �o  �n  	 

 i     I      �m�l�m 0 	doviamenu 	doViaMenu �k o      �j�j 0 	stataname 	stataName�k  �l   O     / Z    .�i�h 1    �g
�g 
uien O    * I   )�f�e
�f .prcsclicnull��� ��� uiel n    % 4   " %�d
�d 
menI m   # $ �  r u n   t m p   f i l e n    " 4    "�c
�c 
menE m     !�b�b  n      4    �a!
�a 
menI! m    "" �## & A p p l e s c r i p t   h e l p e r s  n    $%$ 4    �`&
�` 
menE& m    �_�_ % n    '(' 4    �^)
�^ 
mbri) m    ** �++  U s e r( 4    �],
�] 
mbar, m    �\�\ �e   4    �[-
�[ 
pcap- o    �Z�Z 0 	stataname 	stataName�i  �h   m     ..�                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��   /0/ l     �Y�X�W�Y  �X  �W  0 121 i    343 I      �V5�U�V "0 createmenuitems createMenuItems5 676 o      �T�T 0 	stataname 	stataName7 8�S8 o      �R�R 0 	tmpdofile 	tmpDoFile�S  �U  4 O     G9:9 Z    F;<�Q�P; 1    �O
�O 
uien< O    B=>= k    A?? @A@ l   �NBC�N  B * $ get the command window to the front   C �DD H   g e t   t h e   c o m m a n d   w i n d o w   t o   t h e   f r o n tA EFE I   #�MG�L
�M .prcsclicnull��� ��� uielG n   HIH 4    �KJ
�K 
menIJ m    KK �LL  C o m m a n dI n    MNM 4    �JO
�J 
menEO m    PP �QQ  W i n d o wN n   RSR 4    �IT
�I 
mbriT m    UU �VV  W i n d o wS 4    �HW
�H 
mbarW m    �G�G �L  F XYX I  $ +�FZ�E
�F .prcskprsnull���     ctxtZ b   $ '[\[ m   $ %]] �^^ r w i n d o w   m e n u   a p p e n d   s u b m e n u   " s t U s e r "   " A p p l e s c r i p t   h e l p e r s "\ o   % &�D
�D 
ret �E  Y _`_ I  , 7�Ca�B
�C .prcskprsnull���     ctxta b   , 3bcb b   , 1ded b   , /fgf m   , -hh �ii � w i n d o w   m e n u   a p p e n d   i t e m   " A p p l e s c r i p t   h e l p e r s "   " r u n   t m p   f i l e "   " d o  g o   - .�A�A 0 	tmpdofile 	tmpDoFilee m   / 0jj �kk  "c o   1 2�@
�@ 
ret �B  ` l�?l I  8 A�>m�=
�> .prcskprsnull���     ctxtm b   8 =non m   8 ;pp �qq & w i n d o w   m e n u   r e f r e s ho o   ; <�<
�< 
ret �=  �?  > 4    �;r
�; 
pcapr o    �:�: 0 	stataname 	stataName�Q  �P  : m     ss�                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  2 tut l     �9�8�7�9  �8  �7  u vwv i    xyx I      �6z�5�6  0 dostatacommand doStataCommandz {|{ o      �4�4 0 	stataname 	stataName| }�3} o      �2�2 0 
thecommand 
theCommand�3  �5  y Q     *~�~ w    ��� O    ��� k    �� ��� l   �1���1  �  		activate   � ���  	 a c t i v a t e� ��0� I   �/��
�/ .STscDoCanull���     ctxt� o    �.�. 0 
thecommand 
theCommand� �-��,�- 0 addtoreview addToReview� m    �+
�+ boovtrue�,  �0  � 4    	�*�
�* 
capp� o    �)�) 0 	stataname 	stataName��                                                                                  S5x8  alis    ^  	Tucholsky                      BD ����StataMP.app                                                    ����            ����  
 cu             Stata17   </:Applications:AAApplications:MathTools:Stata17:StataMP.app/    S t a t a M P . a p p   	 T u c h o l s k y  9Applications/AAApplications/MathTools/Stata17/StataMP.app   / ��   R      �(�'�&
�( .ascrerr ****      � ****�'  �&  � O    *��� I    )�%��
�% .sysodlogaskr        TEXT� m     !�� ��� l H a d   t r o u b l e   p a s s i n g   c o m m a n d ( s )   t o   S t a t a   c o m m a n d   w i n d o w� �$��#
�$ 
btns� J   " %�� ��"� m   " #�� ���  C a n c e l�"  �#  � m    ���                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  w ��� l     �!� ��!  �   �  � ��� l     ����  �  �  � ��� i    ��� I      ���� 0 pastetmpstata pasteTmpStata� ��� o      �� 0 	stataname 	stataName� ��� o      �� 0 pasteme pasteMe�  �  � k     ��� ��� q      �� ��� 0 oldclipboard oldClipBoard�  � ��� r     ��� I    ���
� .JonsgClp****    ��� null�  �  � o      �� 0 oldclipboard oldClipBoard� ��� Q    ����� k    h�� ��� I   ���
� .JonspClpnull���     ****� l   ���� c    ��� o    �� 0 pasteme pasteMe� m    �
� 
ctxt�  �  �  � ��
� O    h��� Z    g���	�� 1    �
� 
uien� O    c��� k   % b�� ��� I  % 6���
� .prcsclicnull��� ��� uiel� n  % 2��� 4   / 2��
� 
menI� m   0 1�� ���  C o m m a n d� n   % /��� 4   , /��
� 
menE� m   - .�� ���  W i n d o w� n  % ,��� 4   ) ,��
� 
mbri� m   * +�� ���  W i n d o w� 4   % )��
� 
mbar� m   ' (� �  �  � ��� I  7 <�����
�� .sysodelanull��� ��� nmbr� m   7 8�� ?ə�������  � ��� I  = T�����
�� .prcsclicnull��� ��� uiel� n  = P��� 4   K P���
�� 
menI� l 	 L O������ m   L O�� ��� 
 P a s t e��  ��  � n   = K��� 4   F K���
�� 
menE� m   G J�� ���  E d i t� n  = F��� 4   A F���
�� 
mbri� m   B E�� ���  E d i t� 4   = A���
�� 
mbar� m   ? @���� ��  � ��� l  U U������  � > 8 added delay when seeing odd behavior on machine at work   � ��� p   a d d e d   d e l a y   w h e n   s e e i n g   o d d   b e h a v i o r   o n   m a c h i n e   a t   w o r k� ��� l  U U������  � U O it seems that a delay of under 0.2 seconds makes things weird on fast machines   � ��� �   i t   s e e m s   t h a t   a   d e l a y   o f   u n d e r   0 . 2   s e c o n d s   m a k e s   t h i n g s   w e i r d   o n   f a s t   m a c h i n e s� ��� I  U Z�����
�� .sysodelanull��� ��� nmbr� m   U V�� ?ə�������  � ���� I  [ b�����
�� .prcskprsnull���     ctxt� o   [ ^��
�� 
ret ��  ��  � 4    "���
�� 
pcap� o     !���� 0 	stataname 	stataName�	  �  � m      �                                                                                  sevs  alis    V  	Tucholsky                      BD ����System Events.app                                              ����            ����  
 cu             CoreServices  0/:System:Library:CoreServices:System Events.app/  $  S y s t e m   E v e n t s . a p p   	 T u c h o l s k y  -System/Library/CoreServices/System Events.app   / ��  �
  � R      ������
�� .ascrerr ****      � ****��  ��  � k   p �  I  p u����
�� .JonspClpnull���     **** o   p q���� 0 oldclipboard oldClipBoard��   �� O   v � I  | ���	
�� .sysodlogaskr        TEXT m   | 

 � V H a d   t r o u b l e   p a s t i n g   t o   S t a t a   c o m m a n d   w i n d o w	 ����
�� 
btns J   � � �� m   � � �  C a n c e l��  ��   m   v y�                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  ��  � �� I  � �����
�� .JonspClpnull���     **** o   � ����� 0 oldclipboard oldClipBoard��  ��  �  l     ��������  ��  ��    l     ��������  ��  ��    i     I      ������ "0 stripblanklines stripBlankLines �� o      ���� 0 thetext theText��  ��   k     i   q      !! ��"�� 0 theparas theParas" ��#�� 0 thepara thePara# ��$�� 0 achar aChar$ ��%�� 0 thecleanstuff theCleanStuff% ������ 0 eraseme  ��    &'& r     ()( l    *����* n     +,+ 2   ��
�� 
cpar, o     ���� 0 thetext theText��  ��  ) o      ���� 0 theparas theParas' -.- r    
/0/ J    ����  0 o      ���� 0 thecleanstuff theCleanStuff. 121 X    ^3��43 k    Y55 676 r    898 m    ��
�� boovtrue9 o      ���� 0 eraseme  7 :;: X    I<��=< Z   1 D>?����> H   1 8@@ E  1 7ABA J   1 5CC DED m   1 2FF �GG   E H��H m   2 3II �JJ  	��  B o   5 6���� 0 achar aChar? k   ; @KK LML r   ; >NON m   ; <��
�� boovfalsO o      ���� 0 eraseme  M P��P  S   ? @��  ��  ��  �� 0 achar aChar= n   " %QRQ 2   # %��
�� 
cha R o   " #���� 0 thepara thePara; S��S Z   J YTU����T H   J LVV o   J K���� 0 eraseme  U r   O UWXW l  O RY����Y c   O RZ[Z o   O P���� 0 thepara thePara[ m   P Q��
�� 
TEXT��  ��  X l     \����\ n      ]^]  ;   S T^ o   R S���� 0 thecleanstuff theCleanStuff��  ��  ��  ��  ��  �� 0 thepara thePara4 o    ���� 0 theparas theParas2 _`_ r   _ daba o   _ `��
�� 
ret b l     c����c 1   ` c��
�� 
txdl��  ��  ` d��d L   e iee l  e hf����f c   e hghg o   e f���� 0 thecleanstuff theCleanStuffh m   f g��
�� 
TEXT��  ��  ��   iji l     ��������  ��  ��  j k��k i     #lml I      �������� 0 getosxversion getOSXversion��  ��  m k     nn opo q      qq ������ $0 thefullosversion theFullOSVersion��  p rsr r     tut m     vv �ww  .u l     x����x 1    ��
�� 
txdl��  ��  s yzy O    {|{ r   
 }~} l  
 ���� 1   
 ��
�� 
vers��  ��  ~ o      ���� $0 thefullosversion theFullOSVersion| m    ���                                                                                  MACS  alis    :  	Tucholsky                      BD ����
Finder.app                                                     ����            ����  
 cu             CoreServices  )/:System:Library:CoreServices:Finder.app/    
 F i n d e r . a p p   	 T u c h o l s k y  &System/Library/CoreServices/Finder.app  / ��  z ���� L    �� n    ��� 4    ���
�� 
citm� m    ���� � o    ���� $0 thefullosversion theFullOSVersion��  ��       ��������������  � 	������������������
�� .aevtoappnull  �   � ****�� 0 badfirstarg badFirstArg�� 0 dotmpdofile doTmpDofile�� 0 	doviamenu 	doViaMenu�� "0 createmenuitems createMenuItems��  0 dostatacommand doStataCommand�� 0 pastetmpstata pasteTmpStata�� "0 stripblanklines stripBlankLines�� 0 getosxversion getOSXversion� �� !��������
�� .aevtoappnull  �   � ****�� 0 args  ��  � ����~�}�|�{�z�y�x�w�v�u�t�� 0 args  � 0 numargs numArgs�~ 0 pasteme pasteMe�} 0 dothis doThis�| 0 	tmpdofile 	tmpDoFile�{ 0 howmanystatas howManyStatas�z 0 	thestatas 	theStatas�y 0 thestataname theStataName�x "0 thestataversion theStataVersion�w 0 theolddelims theOldDelims�v 0 theosxversion theOSXVersion�u $0 defaulttmpdofile defaultTmpDoFile�t 0 uiok UIOK� ; E Z�s r�r�q�p ��o�n�m�l�k ��j ��i ��h � � ��g�f � � � ��e�d ��c�b�a�`,27�_��^Q�]�\kp���[�Z�Y��X�W��V�U�T�S
�s 
uien
�r 
vers
�q 
nmbr�p 
�o 0 getosxversion getOSXversion
�n 
long�m 	
�l .miscactvnull��� ��� null
�k 
xppb
�j 
xpcp
�i 
btns
�h .sysodlogaskr        TEXT
�g 
leng
�f 
cobj�e �d 0 badfirstarg badFirstArg�c  �b  
�a .JonsgClp****    ��� null�` "0 stripblanklines stripBlankLines
�_ 
prcs�  
�^ 
pnam
�] .corecnte****       ****
�\ .sysobeepnull��� ��� long
�[ 
bnid
�Z 
ascr
�Y 
txdl
�X 
citm
�W 
pisf�V �U  0 dostatacommand doStataCommand�T 0 pastetmpstata pasteTmpStata�S 0 dotmpdofile doTmpDofile���E�O� *�,E eE�Y fE�UO� *�,�&� eE�Y hUO� Y� Q)j+ �&E�O�� !*j O*��/*�,FO�a a kvl Y "*j O*�a /*�,FOa a a kvl UY��a ,E�O R�a k/E�Oa a a a a v� 
)j+ Y hO�k �a l/E�O�a   �E�Y hY �E�W X   )j+ O)*j !k+ "E�O�a #  � a $a a %kvl UY hO� *a &-a '[a (,\Za )@1E�UO�j *E�O�j  � *j +Oa ,a a -kvl UY 4�k � a .a a /kvl UOPY hO� �a k/a (,E�UO� F*a &�/a 0,E�O_ 1a 2,E�Oa 3kv_ 1a 2,FO�a 4i/E�O�_ 1a 2,FOe*a &�/a 5,FUO�a 6  �a 7 )��l+ 8Y 	)��l+ 9Y 
)���m+ :� �R�Q�P���O�R 0 badfirstarg badFirstArg�Q  �P  �  � �N�M
�N 
btns
�M .sysodlogaskr        TEXT�O � ���kvl U� �L%�K�J���I�L 0 dotmpdofile doTmpDofile�K �H��H �  �G�F�E�G 0 	stataname 	stataName�F 0 	tmpdofile 	tmpDoFile�E 0 dowhat doWhat�J  � �D�C�B�A�@�D 0 	stataname 	stataName�C 0 	tmpdofile 	tmpDoFile�B 0 dowhat doWhat�A 
0 tmpdir  �@ "0 stupidapplefile stupidAppleFile� &@�?�>�=�<�;�:�9�8�7�6�5�4�3�2�1�0�{�/��.�-�,��+�*�)�����(����
�? .sysoexecTEXT���     TEXT
�> 
psxf
�= 
perm
�< .rdwropenshor       file
�; 
set2
�: .rdwrseofnull���     ****
�9 .JonsgClp****    ��� null
�8 
refn
�7 
as  
�6 
utf8�5 
�4 .rdwrwritnull���     ****
�3 
ret 
�2 .rdwrclosnull���     ****�1  �0  
�/ 
btns
�. .sysodlogaskr        TEXT
�- 
capp
�, .miscactvnull��� ��� null�+ 0 	doviamenu 	doViaMenu�* "0 createmenuitems createMenuItems
�) .sysodelanull��� ��� nmbr�( 0 pastetmpstata pasteTmpStata�I-�j E�O��%E�O*�/E�O 0��el O��jl O*j ���� O��l O�j W #X  �j Oa  a a a kvl UO*a �/ *j UO�a   K *�k+ W <X  *��l+ Okj O *�k+ W X  a  a a a kvl UY e�a   1 )�a �%l+  W X  a  a !a a "kvl UY . )�a #�%l+  W X  a  a $a a %kvl U� �'�&�%���$�' 0 	doviamenu 	doViaMenu�& �#��# �  �"�" 0 	stataname 	stataName�%  � �!�! 0 	stataname 	stataName� .� ���*��"�
�  
uien
� 
pcap
� 
mbar
� 
mbri
� 
menE
� 
menI
� .prcsclicnull��� ��� uiel�$ 0� ,*�,E $*�/ *�k/��/�k/��/�k/��/j 
UY hU� �4������ "0 createmenuitems createMenuItems� ��� �  ��� 0 	stataname 	stataName� 0 	tmpdofile 	tmpDoFile�  � ��� 0 	stataname 	stataName� 0 	tmpdofile 	tmpDoFile� s����U�P�K�
]�	�hjp
� 
uien
� 
pcap
� 
mbar
� 
mbri
� 
menE
� 
menI
�
 .prcsclicnull��� ��� uiel
�	 
ret 
� .prcskprsnull���     ctxt� H� D*�,E <*�/ 1*�k/��/��/��/j 
O��%j O�%�%�%j Oa �%j UY hU� �y������  0 dostatacommand doStataCommand� ��� �  ��� 0 	stataname 	stataName� 0 
thecommand 
theCommand�  � � ���  0 	stataname 	stataName�� 0 
thecommand 
theCommand� ������������������
�� 
capp�� 0 addtoreview addToReview
�� .STscDoCanull���     ctxt��  ��  
�� 
btns
�� .sysodlogaskr        TEXT� + �Z*�/ 	��el UW X  � ���kvl 
U� ������������� 0 pastetmpstata pasteTmpStata�� ����� �  ������ 0 	stataname 	stataName�� 0 pasteme pasteMe��  � �������� 0 	stataname 	stataName�� 0 pasteme pasteMe�� 0 oldclipboard oldClipBoard� ������ �������������������������������
����
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
�� .sysodlogaskr        TEXT�� �*j  E�O b��&j O� R*�,E J*�/ ?*�k/��/��/��/j O�j O*�k/�a /�a /�a /j O�j O_ j UY hUW #X  �j Oa  a a a kvl UO�j � ������������ "0 stripblanklines stripBlankLines�� ����� �  ���� 0 thetext theText��  � �������������� 0 thetext theText�� 0 theparas theParas�� 0 thepara thePara�� 0 achar aChar�� 0 thecleanstuff theCleanStuff�� 0 eraseme  � 
����������FI������
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
fE�OY h[OY��O� ��&�6FY h[OY��O�*�,FO��&� ��m���������� 0 getosxversion getOSXversion��  ��  � ���� $0 thefullosversion theFullOSVersion� v�������
�� 
txdl
�� 
vers
�� 
citm�� �*�,FO� *�,E�UO��l/E ascr  ��ޭ