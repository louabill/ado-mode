## Installing the Emacs Ado-mode

### Getting the Code for Emacs

#### Simple Installation Instructions - MELPA

If you are interested in using `ado-mode`, but are not so interested in modifying the code and playing with git repositories, you can install the package (and keep it up to date) using [MELPA](https://melpa.org), the curated package manager for Emacs packages.

Here is how you do this:

1. If you haven't set up [MELPA](https://melpa.org) inside of Emacs, yet
	1. Follow the instructions for [getting started with MELPA](https://melpa.org/#/getting-started)
	1. Quit and restart Emacs.
1. Inside Emacs, be sure that your package list it up-to-date via \
		`M-x package-refresh-contents`.
1. Install the package inside Emacs via `M-x package-install<RET>ado-mode`.
1. Quit and restart Emacs.
1. Try to start `ado-mode` via `M-x ado-mode<RET>`.
	1. If this works, great!
	1. If this fails, add the line \
		`(require 'ado-mode)` \
		to your init file.

This is enough to get the package recognized and syntax highlighting to work. For the full strength of `ado-mode`, you'll want to do a little more. Continue below at [Additional Software](#additional-software).

#### Git Repo/Zip file Installation Instructions

1. Grab the distro from this github site. This will give you a folder called `ado-mode` **or**

2. Grab the highest-numbered/latest release from the [releases](https://https://github.com/louabill/ado-mode/releases) page here. Then unzip the file and rename it to `ado-mode`.
  
3. Decide where you should put the `ado-mode` folder.
	1. Some versions of emacs have a place dedicated for user-installed packages&mdash;if your Emacs has it, you can find it with `C-h v user-emacs-directory`. \
	1. If you have no such folder, make an `emacs` folder in a useful place, and add it to your `load-path`: \
	   `(setq load-path (cons "/Someplace/Useful/emacs" load-path))`

4. After you have a place to hold customizations, move the `ado-mode` folder into it. Remember this location. (For the instructions, I'll assume you put things in this my non-standard useful place:`/Universal/Custom/emacs/ado-mode`.)

5. Add the following two lines to your initialization file: \
  `(add-to-list 'load-path "/Universal/Custom/emacs/ado-mode/lisp")` \
  `(require 'ado-mode)` \
  (The initialization file used to be called the `.emacs` file, but what is named on your system depends on the type of Emacs you are running. If you are unfamiliar with such files, open up Emacs and use `C-h i` to bring up info, then type `m em<RET>m init file` (emacese for _menu emacs_, and then _menu init file_) to read the docs on initialization files.)
  
Using the git repo makes updating easy, as you'll have the whole repo of all the code and its history. Using the zip file will take you back to 1985 with all its resulting headaches.

### Additional Software
To send code to Stata, you might need to install some extra software:
  * If you are using a Mac, this will work out of the box.
  * If you are using MS Windows, you'll need to install [AutoIt](https://www.autoitscript.com/site/autoit/downloads/).
  * If you are using Unix, you will need to get `xsel` and `xdotool`. In Debian/Ubuntu, this scan be done via `sudo apt-get install xsel xdotool`. Other *nixes will be similar.
	 
### Additional Setup Twiddling
`ado-mode` is very customizable, and works fine straight out of the box.

Customizations can be handled by Emacs built-in customization by typing
`M-x customize-group\<RET\> ado`, and playing around with the items in **Ado Main**.
     
#### Changing appearance
	
You will likely want to change the default colors for the font highlighting. Here is a screenshot of the highlighting I use, and which I find quite readable:
	<div style="text-align: center;"><img src="docs4github/ado_highlighting.png" width="880" height="646" alt="highlighing example"></div>
	
The font-locking can be changed either by customizing the `ado-mode` faces directly using `M-x customize-group ado-font-lock`, or by changing the `font-lock` faces which are inherited by the ado-mode via `M-x customize-group font-lock-faces`. I prefer the latter, because changes will then stick for all other programming languages (but it requires knowing which ado-mode names come from which Emacs names). In any case, here are the actual colors from the above screen-shot:
  * comment: firebrick
  * string: magenta
  * harmful: red
  * harmless: blue
  * subcommands: DeepSkyBlue2
  * macros: green3
  * functions: OrangeRed2
  * constants: purple2
  * obsolete: background -- grey80, foreground -- red bold
	
To see what colors are available in Emacs, try `M-x list-colors-display`.
	
Note that specifiying the font is system specific. I use the `apple-dejavu sans mono` font, because I like it. Others like other fonts. You should set the font from within emacs by using `M-x customize`, and then clicking the `Faces`, `Basic Faces`, and then click the `show` button next to **Default** face. This will avoid all the platform specific methods for referring to fonts.

### Troubleshooting

Please use the **Issues** tab on this site to report problems or feature requests.

If you have any trouble with the installation instructions, [drop me a line](&#109;&#97;&#105;&#108;&#116;&#111;:&#98;&#114;&#105;&#115;&#105;&#110;&#103;&#64;&#109;&#97;&#99;&#46;&#99;&#111;&#109;) so that I can fix them.

If you would like keep up with updates, subscribe to [http://twitter.com/statuses/user_timeline/106578815.rss](http://twitter.com/statuses/user_timeline/106578815.rss), which is the @adomode Twitter account. This seems like an easy way to have a very-low-traffic RSS feed without any maintenance. You can also watch this repo, but then you'll get more email then you might want.
