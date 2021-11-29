%%
%% Article Final Gray Print Class, 1 Column
%%
%% Version 1.7
%%

\def\theinfo{Permission to make digital or hard copies of all or part of this work for
personal or classroom use is granted. Copies must
bear this notice and the full citation on the first page.  To copy
otherwise, to republish, to post on servers or to redistribute to lists,
requires prior specific permission from the author(s):}


\usepackage{epsfig}
\usepackage{amssymb}
\usepackage{amsmath}
\usepackage{amsfonts}
\usepackage[usenames,dvipsnames]{color}
\usepackage{graphicx}
\usepackage{float}
\usepackage{pifont}
\usepackage{placeins}

\usepackage{hyperref}

\newenvironment{theindex}
               {\begin{description}\leftmargin10pt\itemindent-10pt\parskip-2pt}
               {\end{description}}
\newcommand\@idxitem{\par\hangindent 20\p@}
\newcommand\subitem{\@idxitem \hspace*{20\p@}}
\newcommand\subsubitem{\@idxitem \hspace*{30\p@}}
\newcommand\indexspace{}
\usepackage{makeidx}
\newcommand{\Index}[1]{{\sl #1}\index{#1}}

\definecolor{lightbox}{gray}{.6}
\definecolor{box-color}{gray}{.6}
\newcommand\boxcolor{\color{box-color}}
\definecolor{text-color}{rgb}{0.0,0.0,0.0}
\definecolor{grey-color}{gray}{.6}
\definecolor{highlight-color}{gray}{.6}
\newdimen\fullwidth \fullwidth=17.5cm
\newdimen\colwidth \colwidth=0.95\textwidth

\def\fboxdef{\setlength{\fboxrule}{1pt}\setlength{\fboxsep}{3pt}}
\fboxdef
\def\myaddress{}
\def\myauthor{}
\def\mydate{}

\newcommand{\bcol}[1]{\textcolor[gray]{0.6}{#1}}      
\newcommand{\colorit}[1]{\textcolor{black}{#1}}
\newcommand{\framecolorbox}[1]{\fbox{#1}}
\newcommand{\textframecolorbox}[1]{\fbox{#1}}
\def\sepa{\hskip3pt\textcolor{grey-color}{$\blacksquare$}\hskip5pt}

\newdimen\abovecaptionskip \abovecaptionskip=5pt
\renewcommand\float@restyle[1]{\expandafter\edef\csname
  fst@#1\endcsname{\expandafter\noexpand\csname
    fs@\float@style\endcsname}%
   \@namedef{#1}{\@nameuse{fst@#1}%
      \@float@setevery{#1}\@float{#1}}%
   \@namedef{#1*}{\@nameuse{fst@#1}%
      \@float@setevery{#1}\@dblfloat{#1}}%
   \expandafter\let\csname end#1\endcsname\float@end
   \expandafter\let\csname end#1*\endcsname\float@dblend
   \expandafter\let\csname @float@c@#1\endcsname=\@caption   
   \@ifundefined{@float@every@#1}{%
     \expandafter\newtoks\csname @float@every@#1\endcsname}{}%
   \@nameuse{@float@every@#1}={}}
\newcommand\float@restylecap[1]{\expandafter\edef\csname
  fst@#1\endcsname{\expandafter\noexpand\csname
    fs@\float@style\endcsname}%
   \@namedef{#1}{\@nameuse{fst@#1}%
      \@float@setevery{#1}\@float{#1}}%
   \@namedef{#1*}{\@nameuse{fst@#1}%
      \@float@setevery{#1}\@dblfloat{#1}}%
   \expandafter\let\csname end#1\endcsname\float@end
   \expandafter\let\csname end#1*\endcsname\float@dblend
   \expandafter\let\csname @float@c@#1\endcsname=\float@caption
   \@ifundefined{@float@every@#1}{%
     \expandafter\newtoks\csname @float@every@#1\endcsname}{}%
   \@nameuse{@float@every@#1}={}}
\newcommand\fs@topcapruled{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@ruled
  \def\@fs@pre{\textcolor{box-color}{\hrule height.8pt depth0pt }\kern2pt}%
  \def\@fs@post{\kern3pt\textcolor{box-color}{\hrule}\relax}%
  \def\@fs@mid{\kern2pt\textcolor{box-color}{\hrule}\kern5pt}%
  \let\@fs@iftopcapt\iftrue}
\newcommand\fs@topcapplain{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@ruled
  \def\@fs@pre{\kern5pt}%
  \def\@fs@post{\kern5pt}%
  \def\@fs@mid{\kern10pt}%
  \let\@fs@iftopcapt\iftrue}
\renewcommand\fs@boxed{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@plain
  \newdimen\hsizepl
  \hsizepl=0.9\hsize
  \def\@fs@pre{\setbox\@currbox\vbox{\hbadness10000
    \vbox{\advance\hsizepl by21pt
      \textcolor{box-color}{\hrule height 1pt}\hbox to\hsizepl{\textcolor{box-color}{\vrule width 1pt}\kern10pt
        \vbox{\kern10pt\box\@currbox\kern10pt}\hskip-12pt\textcolor{box-color}{\vrule width 1pt}}
          \vskip-11pt\textcolor{box-color}{\hrule height 1pt}}}}%
  \def\@fs@mid{\kern2pt}%
  \def\@fs@post{}\let\@fs@iftopcapt\iffalse}
\renewcommand\fs@ruled{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@plain
  \newdimen\hsizepl
  \hsizepl=0.9\hsize
  \def\@fs@pre{\setbox\@currbox\vbox{\hbadness10000
    \vbox{\advance\hsizepl by21pt
      \textcolor{box-color}{\hrule}\kern2.5pt\textcolor{box-color}{\hrule}\hbox to\hsizepl{\kern10pt
        \vbox{\kern10pt\box\@currbox\kern10pt}\kern9pt}
          \vskip-11pt\textcolor{box-color}{\hrule}\kern2.5pt\textcolor{box-color}{\hrule}}}}%
  \def\@fs@mid{\kern2pt}%
  \def\@fs@post{}\let\@fs@iftopcapt\iffalse}
\newcommand\fs@bared{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@plain
  \newdimen\hsizepl
  \hsizepl=0.9\hsize
  \def\@fs@pre{\setbox\@currbox\vbox{\hbadness10000
    \bcol{\strut\vrule width 5pt}\vbox{\advance\hsizepl by21pt
      \hbox to\hsizepl{
        \vbox{\kern10pt\box\@currbox\kern10pt}\kern9pt}
          }}}%
  \def\@fs@mid{\kern2pt}%
  \def\@fs@post{}\let\@fs@iftopcapt\iffalse}

\def\baseblock#1#2#3#4#5{
  \vbox{\setbox0=\hbox{#5}
    \offinterlineskip
    \hbox{\copy0
      \dimen0=\ht0
      \advance\dimen0 by -#1
      {\boxcolor\vrule height \dimen0 width#2}}
    \hbox{\hskip#3\dimen0=\wd0
      \advance\dimen0 by -#3
      \advance\dimen0 by #2
      {\boxcolor\vrule height #4 width \dimen0}}
    }
}
\def\loopblock#1#2{
\begingroup
  \dimen2=0.4pt
  \def\doblock{
    \setbox2\baseblock
      {\count1\dimen2}{0.4pt}{\count1\dimen2}{0.4pt}{\box2}}
    \setbox2=\vbox{#1}
    \count1=0
    \loop
      \advance\count1 by 1
      \doblock
      \ifnum\count1<#2
    \repeat
    \box2
\endgroup}

\def\framebox#1#2#3{
  \vbox{{\boxcolor\hrule height #2}
    \hbox{{\boxcolor\vrule width #2}
      \hskip#1
      \vbox{\vskip#1{}
        #3
        \vskip#1}
      \hskip#1
      {\boxcolor\vrule width #2}}
    {\boxcolor\hrule height #2}}
}

\def\loopblock#1#2{
\begingroup
  \dimen2=0.4pt
  \def\doblock{
    \setbox2\baseblock
      {\count1\dimen2}{0.4pt}{\count1\dimen2}{0.4pt}{\box2}}
    \setbox2=\vbox{#1}
    \count1=0
    \loop
      \advance\count1 by 1
      \doblock
      \ifnum\count1<#2
    \repeat
    \box2
\endgroup}


\newcommand\fs@blocked{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@plain
  \newdimen\hsizepl
  \hsizepl=0.88\hsize
  \def\@fs@pre{\setbox\@currbox\vbox{\hbadness10000
    \loopblock{\framebox{5mm}{0.4pt}{
      \hbox to\hsizepl{
        \hskip-5pt\vbox{\box\@currbox}}
          }}{10}}}%
  \def\@fs@mid{\kern2pt}%
  \def\@fs@post{}\let\@fs@iftopcapt\iffalse}

\newcommand\fs@shaded{\def\@fs@cfont{\bfseries}\let\@fs@capt\floatc@plain
  \newdimen\hsizepl
  \hsizepl=0.88\hsize
  \def\@fs@pre{\setbox\@currbox\vbox{\hbadness10000
    \baseblock{4pt}{4pt}{5pt}{6pt}{\fboxdef\fcolorbox{box-color}{white}{
      \hbox to\hsizepl{
        \hskip-10pt\vbox{\box\@currbox\kern5pt}\hskip5pt}
          }}}}%
  \def\@fs@mid{\kern2pt}%
  \def\@fs@post{}\let\@fs@iftopcapt\iffalse}

\long\def\@makecaption#1#2{
   \vskip 10pt
   \setbox\@tempboxa\hbox{\textbf{#1 #2}}
   \ifdim \wd\@tempboxa >0.85\hsize % IF longer than one line:
       \hskip0.03\hsize\parbox{0.85\hsize}{\textbf{#1 #2}}\par                %   THEN set as ordinary paragraph.
     \else                      %   ELSE  center.
       \hbox to 0.85\hsize{\hfil\box\@tempboxa\hfil}
   \fi}
\renewcommand\floatc@plain[2]{
   \vskip 10pt
   \setbox\@tempboxa\hbox{\textbf{#1 #2}}
   \ifdim \wd\@tempboxa >0.85\hsize % IF longer than one line:
       \hskip0.03\hsize\parbox{0.85\hsize}{\textbf{#1 #2}}\par                %   THEN set as ordinary paragraph.
     \else                      %   ELSE  center.
       \hbox to 0.85\hsize{\hfil\box\@tempboxa\hfil}
   \fi}
\renewcommand\floatc@ruled[2]{
   \vskip 3pt
   \setbox\@tempboxa\hbox{\textbf{#1 #2}}
   \ifdim \wd\@tempboxa >0.85\hsize % IF longer than one line:
       \hskip0.03\hsize\parbox{0.85\hsize}{\textbf{#1 #2}}\par                %   THEN set as ordinary paragraph.
     \else                      %   ELSE  center.
       \hbox to 0.85\hsize{\hfil\box\@tempboxa\hfil}
   \fi
   \vskip 3pt}
\@ifundefined{figure}{\newcounter {figure}} % this is for LaTeX2e

\def\figureNnone{\def\fnum@figure{\hskip0pt}}
\def\figureNArabic{\def\fnum@figure{{\sc \figlabel \, \arabic{figure}\sepa}}}
\def\figureNUroman{\def\fnum@figure{{\sc \figlabel \, \Roman{figure}\sepa}}}
\def\figureNLroman{\def\fnum@figure{{\sc \figlabel \, \roman{figure}\sepa}}}
\def\figureNUalpha{\def\fnum@figure{{\sc \figlabel \, \Alph{figure}\sepa}}}
\def\figureNLalpha{\def\fnum@figure{{\sc \figlabel \, \alph{figure}\sepa}}}
\def\fps@figure{tbp}
\def\ftype@figure{2}
\def\ext@figure{lof}
\def\fnum@figure{{\sc \figlabel \, \arabic{figure}\sepa}}
\def\figure{\@float{figure}}
\let\endfigure\end@float
\@namedef{figure*}{\@dblfloat{figure}}
\@namedef{endfigure*}{\end@dblfloat}

\def\figureBboxed{\floatstyle{boxed}\float@restyle{figure}}
\def\figureBruled{\floatstyle{ruled}\float@restyle{figure}}
\def\figureBbared{\floatstyle{bared}\float@restyle{figure}}
\def\figureBplain{\floatstyle{plain}\float@restyle{figure}}
\def\figureBblocked{\floatstyle{blocked}\float@restyle{figure}}
\def\figureBshaded{\floatstyle{shaded}\float@restyle{figure}}
\def\figureBtopcapruled{\floatstyle{topcapruled}\float@restylecap{figure}}
\def\figureBtopcapplain{\floatstyle{topcapplain}\float@restylecap{figure}}

\@ifundefined{table}{\newcounter {table}} % this is for LaTeX2e

\def\tableNnone{\def\fnum@table{\hskip0pt}}
\def\tableNArabic{\def\fnum@table{{\sc \tablabel \, \thetable\sepa}}}
\def\tableNUroman{\def\fnum@table{{\sc \tablabel \, \Roman{table}\sepa}}}
\def\tableNLroman{\def\fnum@table{{\sc \tablabel \, \roman{table}\sepa}}}
\def\tableNUalpha{\def\fnum@table{{\sc \tablabel \, \Alph{table}\sepa}}}
\def\tableNLalpha{\def\fnum@table{{\sc \tablabel \, \alph{table}\sepa}}}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{{\sc \tablabel \, \thetable\sepa}}
\def\table{\let\normalsize\footnotesize \normalsize\@float{table}}
\let\endtable\end@float
\@namedef{table*}{\@dblfloat{table}}
\@namedef{endtable*}{\end@dblfloat}

\def\tableBboxed{\floatstyle{boxed}\float@restyle{table}}
\def\tableBruled{\floatstyle{ruled}\float@restyle{table}}
\def\tableBbared{\floatstyle{bared}\float@restyle{table}}
\def\tableBplain{\floatstyle{plain}\float@restyle{table}}
\def\tableBblocked{\floatstyle{blocked}\float@restyle{table}}
\def\tableBshaded{\floatstyle{shaded}\float@restyle{table}}
\def\tableBtopcapruled{\floatstyle{topcapruled}\float@restylecap{table}}
\def\tableBtopcapplain{\floatstyle{topcapplain}\float@restylecap{table}}

\@ifundefined{example}{\newcounter {example}} % this is for LaTeX2e

\def\exampleNnone{\def\fnum@example{\hskip0pt}}
\def\exampleNArabic{\def\fnum@example{{\sc \exlabel \,  \theexample\sepa}}}
\def\exampleNUroman{\def\fnum@example{{\sc \exlabel \,  \Roman{example}\sepa}}}
\def\exampleNLroman{\def\fnum@example{{\sc \exlabel \,  \roman{example}\sepa}}}
\def\exampleNUalpha{\def\fnum@example{{\sc \exlabel \,  \Alph{example}\sepa}}}
\def\exampleNLalpha{\def\fnum@example{{\sc \exlabel \,  \alph{example}\sepa}}}
\def\fps@example{tbp}
\def\ftype@example{2}
\def\ext@example{lot}
\def\fnum@example{{\sc \exlabel \,  \theexample\sepa}}
\def\example{\@float{example}}
\let\endexample\end@float
\@namedef{example*}{\@dblfloat{example}}
\@namedef{endexample*}{\end@dblfloat}

\def\exampleBboxed{\floatstyle{boxed}\float@restyle{example}}
\def\exampleBruled{\floatstyle{ruled}\float@restyle{example}}
\def\exampleBbared{\floatstyle{bared}\float@restyle{example}}
\def\exampleBplain{\floatstyle{plain}\float@restyle{example}}
\def\exampleBshaded{\floatstyle{shaded}\float@restyle{example}}
\def\exampleBblocked{\floatstyle{blocked}\float@restyle{example}}
\def\exampleBtopcapruled{\floatstyle{topcapruled}\float@restylecap{example}}
\def\exampleBtopcapplain{\floatstyle{topcapplain}\float@restylecap{example}}

\@ifundefined{graph}{\newcounter {graph}} % this is for LaTeX2e

\def\graphNnone{\def\fnum@graph{\hskip0pt}}
\def\graphNArabic{\def\fnum@graph{{\sc \graphlabel \, \arabic{graph}\sepa}}}
\def\graphNUroman{\def\fnum@graph{{\sc \graphlabel \, \Roman{graph}\sepa}}}
\def\graphNLroman{\def\fnum@graph{{\sc \graphlabel \, \roman{graph}\sepa}}}
\def\graphNUalpha{\def\fnum@graph{{\sc \graphlabel \, \Alph{graph}\sepa}}}
\def\graphNLalpha{\def\fnum@graph{{\sc \graphlabel \, \alph{graph}\sepa}}}
\def\fps@graph{tbp}
\def\ftype@graph{2}
\def\ext@graph{lot}
\def\fnum@graph{{\sc \graphlabel \, \thegraph\sepa}}
\def\graph{\@float{graph}}
\let\endgraph\end@float
\@namedef{graph*}{\@dblfloat{graph}}
\@namedef{endgraph*}{\end@dblfloat}

\def\graphBboxed{\floatstyle{boxed}\float@restyle{graph}}
\def\graphBruled{\floatstyle{ruled}\float@restyle{graph}}
\def\graphBbared{\floatstyle{bared}\float@restyle{graph}}
\def\graphBplain{\floatstyle{plain}\float@restyle{graph}}
\def\graphBblocked{\floatstyle{blocked}\float@restyle{graph}}
\def\graphBshaded{\floatstyle{shaded}\float@restyle{graph}}
\def\graphBtopcapruled{\floatstyle{topcapruled}\float@restylecap{graph}}
\def\graphBtopcapplain{\floatstyle{topcapplain}\float@restylecap{graph}}

\@ifundefined{definition}{\newcounter {definition}} % this is for LaTeX2e

\def\definitionNnone{\def\fnum@definition{\hskip0pt}}
\def\definitionNArabic{\def\fnum@definition{{\sc \deflabel \, \thedefinition\sepa}}}
\def\definitionNUroman{\def\fnum@definition{{\sc \deflabel \, \Roman{definition}\sepa}}}
\def\definitionNLroman{\def\fnum@definition{{\sc \deflabel \, \roman{definition}\sepa}}}
\def\definitionNUalpha{\def\fnum@definition{{\sc \deflabel \, \Alph{definition}\sepa}}}
\def\definitionNLalpha{\def\fnum@definition{{\sc \deflabel \, \alph{definition}\sepa}}}
\def\fps@definition{tbp}
\def\ftype@definition{2}
\def\ext@definition{lot}
\def\fnum@definition{{\sc \deflabel \, \thedefinition\sepa}}
\def\definition{\@float{definition}}
\let\enddefinition\end@float
\@namedef{definition*}{\@dblfloat{definition}}
\@namedef{enddefinition*}{\end@dblfloat}

\def\definitionBboxed{\floatstyle{boxed}\float@restyle{definition}}
\def\definitionBruled{\floatstyle{ruled}\float@restyle{definition}}
\def\definitionBbared{\floatstyle{bared}\float@restyle{definition}}
\def\definitionBplain{\floatstyle{plain}\float@restyle{definition}}
\def\definitionBblocked{\floatstyle{blocked}\float@restyle{definition}}
\def\definitionBshaded{\floatstyle{shaded}\float@restyle{definition}}
\def\definitionBtopcapruled{\floatstyle{topcapruled}\float@restylecap{definition}}
\def\definitionBtopcapplain{\floatstyle{topcapplain}\float@restylecap{definition}}

\@ifundefined{plot}{\newcounter {plot}} % this is for LaTeX2e

\def\plotNnone{\def\fnum@plot{\hskip0pt}}
\def\plotNArabic{\def\fnum@plot{{\sc \plotlabel \, \theplot\sepa}}}
\def\plotNUroman{\def\fnum@plot{{\sc \plotlabel \, \Roman{plot}\sepa}}}
\def\plotNLroman{\def\fnum@plot{{\sc \plotlabel \, \roman{plot}\sepa}}}
\def\plotNUalpha{\def\fnum@plot{{\sc \plotlabel \, \Alph{plot}\sepa}}}
\def\plotNLalpha{\def\fnum@plot{{\sc \plotlabel \, \alph{plot}\sepa}}}
\def\fps@plot{tbp}
\def\ftype@plot{2}
\def\ext@plot{lot}
\def\fnum@plot{{\sc \plotlabel \, \theplot\sepa}}
\def\plot{\@float{plot}}
\let\endplot\end@float
\@namedef{plot*}{\@dblfloat{plot}}
\@namedef{endplot*}{\end@dblfloat}

\def\plotBboxed{\floatstyle{boxed}\float@restyle{plot}}
\def\plotBruled{\floatstyle{ruled}\float@restyle{plot}}
\def\plotBbared{\floatstyle{ruled}\float@restyle{plot}}
\def\plotBplain{\floatstyle{plain}\float@restyle{plot}}
\def\plotBblocked{\floatstyle{blocked}\float@restyle{plot}}
\def\plotBshaded{\floatstyle{shaded}\float@restyle{plot}}
\def\plotBtopcapruled{\floatstyle{topcapruled}\float@restylecap{plot}}
\def\plotBtopcapplain{\floatstyle{topcapplain}\float@restylecap{plot}}

\@ifundefined{algorithm}{\newcounter {algorithm}} % this is for LaTeX2e

\def\algorithmNnone{\def\fnum@algorithm{\hskip0pt}}
\def\algorithmNArabic{\def\fnum@algorithm{{\sc \alglabel \, \thealgorithm\sepa}}}
\def\algorithmNUroman{\def\fnum@algorithm{{\sc \alglabel \, \Roman{algorithm}\sepa}}}
\def\algorithmNLroman{\def\fnum@algorithm{{\sc \alglabel \, \roman{algorithm}\sepa}}}
\def\algorithmNUalpha{\def\fnum@algorithm{{\sc \alglabel \, \Alph{algorithm}\sepa}}}
\def\algorithmNLalpha{\def\fnum@algorithm{{\sc \alglabel \, \alph{algorithm}\sepa}}}
\def\fps@algorithm{tbp}
\def\ftype@algorithm{2}
\def\ext@algorithm{lot}
\def\fnum@algorithm{\textcolor{blue-color}{\alglabel \, \thealgorithm\sepa}}
\def\algorithm{\@float{algorithm}}
\let\endalgorithm\end@float
\@namedef{algorithm*}{\@dblfloat{algorithm}}

\def\algorithmBboxed{\floatstyle{boxed}\float@restyle{algorithm}}
\def\algorithmBruled{\floatstyle{ruled}\float@restyle{algorithm}}
\def\algorithmBbared{\floatstyle{bared}\float@restyle{algorithm}}
\def\algorithmBplain{\floatstyle{plain}\float@restyle{algorithm}}
\def\algorithmBblocked{\floatstyle{blocked}\float@restyle{algorithm}}
\def\algorithmBshaded{\floatstyle{shaded}\float@restyle{algorithm}}
\def\algorithmBtopcapruled{\floatstyle{topcapruled}\float@restylecap{algorithm}}
\def\algorithmBtopcapplain{\floatstyle{topcapplain}\float@restylecap{algorithm}}


%%%%%%%%%%%%%%%%%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{article1} [2009/09/15 BSSLAB 2009 and 1996/07/03 ACM Transactions class]
\RequirePackage{latexsym}
%aiellom{
\RequirePackage{url}

% Do not change the following! Use the appropriate acmtocl, acmtods, ... option
\def\@acmVolume{V} %the volume
\def\@acmNumber{N} %the number
\def\@acmYear{YY}  %the last two digits of the year, 
\def\@acmMonth{Month}  %the month
\def\@journalName{\parbox{0.8\textwidth}{\vskip10pt\textcolor{grey-color}{\theinfo}}} %the name of the ACM journal
\def\@journalNameShort{jn} %the acronym of the ACM journal
\def\@permissionCodeOne{0000-0000} %the permission code of the ACM journal
\def\@permissionCodeTwo{0000} %the permission code of the ACM journal part 2
\def\@pageCode{\acmPageCode} %the first page of the article in 4 digits


\newif\if@acmjacm
\newif\if@acmtocl
\newif\if@acmtodaes
\newif\if@acmtods
\newif\if@acmtogs
\newif\if@acmtoms
\newif\if@acmtoplas

\DeclareOption{acmnow}{
  \typeout{}
  \typeout{Directly generating the Month and Year for footers from the clock.}
  \def\@acmYear{\yearTwoDigits}  
  \def\@acmMonth{\monthWord}  
}

\DeclareOption{acmjacm}{
  \typeout{}
  \typeout{Using ACM, JACM's option: 2001/06/01 by Marco Aiello et al.}
  \typeout{}
  %\global\@acmjacmfalse  
  \global\@acmtoclfalse
  \global\@acmtodaesfalse
  \global\@acmtodsfalse
  \global\@acmtogsfalse
  \global\@acmtomsfalse
  \global\@acmtoplasfalse
  \global\@acmjacmtrue
  \def\@journalName{Journal of the ACM}
  \def\@journalNameShort{jacm}        
  \def\@permissionCodeOne{0004-5411} 
  \def\@permissionCodeTwo{0100} 
}

\DeclareOption{acmtocl}{
  \typeout{}
  \typeout{Using ACM, TOCL's option: 2001/06/01 by Marco Aiello et al.}
  \typeout{}
  \global\@acmjacmfalse  
  \global\@acmtoclfalse
  \global\@acmtodaesfalse
  \global\@acmtodsfalse
  \global\@acmtogsfalse
  \global\@acmtomsfalse
  \global\@acmtoplasfalse
  \global\@acmtocltrue
  \def\@journalName{ACM Transactions on Computational Logic}
  \def\@journalNameShort{tocl}        
  \def\@permissionCodeOne{1529-3785} 
  \def\@permissionCodeTwo{0700} 
}

\DeclareOption{acmtodaes}{
  \typeout{}
  \typeout{Using ACM, TODAES option: 2001/06/01 by Marco Aiello et al.}
  \typeout{}
  \global\@acmjacmfalse  
  \global\@acmtoclfalse
  %\global\@acmtodaesfalse
  \global\@acmtodsfalse
  \global\@acmtogsfalse
  \global\@acmtomsfalse
  \global\@acmtoplasfalse
  \global\@acmtodaestrue
  \def\@journalName{ACM Transactions on Design Automation of Electronic Systems}
  \def\@journalNameShort{todaes}        
  \def\@permissionCodeOne{1084-4309} 
  \def\@permissionCodeTwo{0400} 
}

\DeclareOption{acmtods}{
  \typeout{}
  \typeout{Using ACM, TODS's option: 2001/06/01 by Marco Aiello et al.}
  \typeout{}
  \global\@acmjacmfalse  
  \global\@acmtoclfalse
  \global\@acmtodaesfalse
  %\global\@acmtodsfalse
  \global\@acmtogsfalse
  \global\@acmtomsfalse
  \global\@acmtoplasfalse
  \global\@acmtodstrue
  \def\@journalName{ACM Transactions on Database Systems}
  \def\@journalNameShort{tods}        
  \def\@permissionCodeOne{0362-5915} 
  \def\@permissionCodeTwo{0300} 
}

\DeclareOption{acmtogs}{
  \typeout{}
  \typeout{Using ACM, TOGS's option: 2001/06/01 by Marco Aiello et al.}
  \typeout{}
  \global\@acmjacmfalse  
  \global\@acmtoclfalse
  \global\@acmtodaesfalse
  \global\@acmtodsfalse
  %\global\@acmtogsfalse
  \global\@acmtomsfalse
  \global\@acmtoplasfalse
  \global\@acmtogstrue
  \def\@journalName{ACM Transactions on Graphics}
  \def\@journalNameShort{togs}        
  \def\@permissionCodeOne{0730-0301} 
  \def\@permissionCodeTwo{0100} 
}

\DeclareOption{acmtoms}{
  \typeout{}
  \typeout{Using ACM, TOMS's option: 2001/06/01 by Marco Aiello et al.}
  \typeout{}
  \global\@acmjacmfalse  
  \global\@acmtoclfalse
  \global\@acmtodaesfalse
  \global\@acmtodsfalse
  \global\@acmtogsfalse
  %\global\@acmtomsfalse
  \global\@acmtoplasfalse
  \global\@acmtomstrue
  \def\@journalName{ACM Transactions on Mathematical Software}
  \def\@journalNameShort{toms}        
  \def\@permissionCodeOne{0098-3500} 
  \def\@permissionCodeTwo{1200} 
}

\DeclareOption{acmtoplas}{
  \typeout{}
  \typeout{Using ACM, TOPLAS option: 2001/06/01 by Marco Aiello et al.}
  \typeout{}
  \global\@acmjacmfalse  
  \global\@acmtoclfalse
  \global\@acmtodaesfalse
  \global\@acmtodsfalse
  \global\@acmtogsfalse
  \global\@acmtomsfalse
  %\global\@acmtoplasfalse
  \global\@acmtoplastrue
  \def\@journalName{ACM Transactions on Programming Languages and Systems}
  \def\@journalNameShort{toplas}        
  \def\@permissionCodeOne{0164-0925} 
  \def\@permissionCodeTwo{0500} 
}
%}aiellom



\if@compatibility\else
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}%
    \def\special@paper{210mm,297mm}}
\DeclareOption{a5paper}
   {\setlength\paperheight {210mm}%
    \setlength\paperwidth  {148mm}%
    \def\special@paper{148mm,210mm}}
\DeclareOption{b5paper}
   {\setlength\paperheight {250mm}%
    \setlength\paperwidth  {176mm}%
    \setlength\voffset     {-15mm}%
    \setlength\hoffset     {-20mm}%
    \def\special@paper{176mm,250mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}%
    \def\special@paper{8.5in,11in}}
\DeclareOption{legalpaper}
   {\setlength\paperheight {14in}%
    \setlength\paperwidth  {8.5in}%
    \def\special@paper{8.5in,14in}}
\DeclareOption{executivepaper}
   {\setlength\paperheight {10.5in}%
    \setlength\paperwidth  {7.25in}%
    \def\special@paper{7.25in,10.5in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
\fi

\DeclareOption{checkMargin}{\setlength\overfullrule{5pt}}
\DeclareOption{final}{\setlength\overfullrule{0pt}}

\DeclareOption{oneside}{\@twosidefalse \@mparswitchfalse}
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}

\DeclareOption{10pt}{\def\@ptsize{0}} %needed for amssymbols.sty 
\DeclareOption{11pt}{\ClassError{acmtrans}{11pt style not supported}
                        {ACM transactions documents can be set in 10pt only}}
\DeclareOption{12pt}{\ClassError{acmtrans}{11pt style not supported}
                        {ACM transactions documents can be set in 10pt only}}
\newif\if@hyperref
\DeclareOption{hyperref}{%
        \def\pages{\pageref{@firstpg}--\pageref{@lastpg}}%
        \def\mypage{\thepage}%
        \def\@getpagenum#1#2#3#4{#2}%
        \def\pdfinfo#1#2{\pdfmark{pdfmark=/DOCINFO,Title=#1,Author=#2}}
        \global\@hyperreftrue
        }
\DeclareOption{nohyperref}{
                \def\pages{\pageref{@firstpg}--\pageref{@lastpg}}%
                \def\@getpagenum#1#2{#2}%
                \def\mypage{\thepage}%
                \def\pdfinfo#1#2{}%
                \def\pdfbookmark#1#2{}%
                \global\@hyperreffalse
                }
\DeclareOption{notfinal}{
                \def\pages{BD}%
                \def\mypage{TBD}%
                \def\@getpagenum#1#2{#2}%
                \def\pdfinfo#1#2{}%
                \def\pdfbookmark#1#2{}%
                }
\DeclareOption{omitline}{\def\@abstractbottom{\relax}}
\DeclareOption{dontomitline}{\def\@abstractbottom{\if@acmjacm\else\hbox{\vrule height .2pt width 30pc}\fi}}
\ExecuteOptions{twoside,notfinal,10pt,dontomitline,nohyperref,letterpaper} % defaults



\ProcessOptions

%{aiellom to automatize the issue specific data
\def\acmVolume#1{\def\@acmVolume{#1}}
\def\acmNumber#1{\def\@acmNumber{#1}}
\def\acmYear#1{\def\@acmYear{#1}}
\def\acmMonth#1{\def\@acmMonth{#1}}


% Command to get the year from the system and display the last two digits
\newcommand{\ignoretwo}[2]{}
\newcommand{\yearTwoDigits}{\expandafter\ignoretwo\the\year}
%To transform the month number in its name in English
\newcommand{\monthWord}{\ifnum\the\month=1 January\fi\ifnum\the\month=2 February\fi\ifnum\the\month=3 March\fi\ifnum\the\month=4 April\fi\ifnum\the\month=5
May\fi\ifnum\the\month=6
June\fi\ifnum\the\month=7 July\fi\ifnum\the\month=8 August\fi\ifnum\the\month=9 September\fi\ifnum\the\month=10 October\fi\ifnum\the\month=11
November\fi\ifnum\the\month=12 December\fi}
% overright the \@setref definition, so that if a reference is undefined 
% then it returns a number 0 and then the usual double boldface 
% question marks ??
% this is necessary for the \acmPageCode command, otherwise it gives an error 
% everytime the aux file is not there
\def\@setref#1#2#3{%
  \ifx#1\relax
    \number 0\relax
    \protect\G@refundefinedtrue
    \nfss@text{\reset@font\bfseries ??}%
    \@latex@warning{Reference `#3' on page \thepage \space undefined}%
  \else
    \expandafter#2#1\null
  \fi}
%make the code a four digits string based on the first page number
\newcommand{\acmPageCode}{\bgroup
  \newcount\c@tempo
  \setcounter{tempo}{\number\pageref{@firstpg}}\ifnum \c@tempo<1000 0\fi\ifnum \c@tempo<100 0\fi\ifnum \c@tempo<10 0\fi\ifnum \c@tempo<1 0\fi\pageref{@firstpg}
  \egroup
}
%}aiellom



\lineskip 1pt \normallineskip 1pt
\def\baselinestretch{1}

\renewcommand\normalsize{%
  \@setfontsize\normalsize\@xpt\@xiipt
  \abovedisplayskip 6pt plus2pt minus1pt\belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip 6pt plus0pt minus 3pt
  \belowdisplayshortskip 6pt plus0pt minus3pt\let\@listi\@listI} 

\newcommand\small{%
  \@setfontsize\small\@ixpt{11pt}%
  \abovedisplayskip 5pt plus 2pt minus 1pt\belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip 5pt plus0pt minus2pt\belowdisplayshortskip 5pt plus0pt
      minus 2pt
  \def\@listi{\leftmargin\leftmargini \topsep 5pt plus 2pt minus 1pt\parsep 0pt
    plus .7pt 
  \itemsep 1.6pt plus .8pt}}
\newcommand\footnotesize{%
%   \@setfontsize\footnotesize\@viiipt{10pt}
 \@setsize\footnotesize{10pt}\viiipt\@viiipt
  \abovedisplayskip 4pt plus 1pt minus 0pt\belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip 4pt plus 0pt minus 1pt\belowdisplayshortskip 4pt plus
       0pt minus 1pt
  \def\@listi{\leftmargin\leftmargini \topsep 4pt plus 1pt minus
     0pt\parsep 0pt plus .5pt 
     \itemsep 1pt plus .7pt}}

\newcommand\scriptsize{\@setfontsize\scriptsize\@viipt\@viiipt}
\newcommand\tiny{\@setfontsize\tiny\@vpt\@vipt}
\newcommand\large{\@setfontsize\large\@xiipt{14}}
\newcommand\Large{\@setfontsize\Large\@xivpt{18}}
\newcommand\LARGE{\@setfontsize\LARGE\@xviipt{20}}
\newcommand\huge{\@setfontsize\huge\@xxpt{25}}
\newcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}

\normalsize 

 \oddsidemargin .75in \evensidemargin .75in \marginparwidth .5in 
 \marginparsep .125in 
 \topmargin .25in \headheight 12pt\headsep 16pt
  %% not in latex2e  \footheight 10pt
  \footskip 15pt 

\textheight 47pc \textwidth 30pc \columnsep 10pt \columnseprule 0pt 
% next five lines added by K.R. Apt, March 20, 01
\advance\textheight-2.6pt
\newdimen\normaltextheight
\setlength\normaltextheight{\textheight}
%\renewcommand\rmdefault{pnc}
%\renewcommand\sfdefault{phv}


\footnotesep 7pt
\skip\footins 15pt plus 4pt minus 3pt 
\floatsep 12pt plus 2pt minus 2pt 
\textfloatsep \floatsep 
\intextsep 1pc plus 1pc 
%%% not in 2e %% \@maxsep 1pc 
%%% not in 2e %% \@dblmaxsep 20pt 
\dblfloatsep 12pt plus 2pt minus 2pt 
\dbltextfloatsep 20pt plus 2pt minus 4pt 
\@fptop 0pt plus 1fil \@fpsep 1pc plus 2fil \@fpbot 0pt plus 1fil 
\@dblfptop 0pt plus 1fil \@dblfpsep 8pt plus 2fil \@dblfpbot 0pt plus 1fil
\marginparpush 6pt 

\parskip 0pt plus .1pt \parindent 10pt \partopsep 0pt 
\@lowpenalty 51 \@medpenalty 151 \@highpenalty 301 
\@beginparpenalty -\@lowpenalty \@endparpenalty -\@lowpenalty \@itempenalty
-\@lowpenalty 

\def\sepb{\hskip5pt\textcolor{grey-color}{$\blacksquare$}\hskip-2pt}

\def\part{\@ucheadtrue
 \@startsection{part}{9}{\z@}{-10pt plus -4pt minus 
 -2pt}{4pt}{\reset@font\normalsize\sffamily}}
\def\section{\@ucheadtrue
 \@startsection{section}{1}{\z@}{-10pt plus -4pt minus 
 -2pt}{4pt}{\reset@font\normalsize\sffamily}}
\def\subsection{\@ucheadfalse
 \@startsection{subsection}{2}{\z@}{-8pt plus -2pt minus 
 -1pt}{4pt}{\reset@font\normalsize\sffamily}}
\def\subsubsection{\@ucheadfalse
 \@startsection{subsubsection}{3}{\parindent}{6pt plus 
1pt}{-5pt}{\reset@font\normalsize\itshape}}
\def\paragraph{\@ucheadfalse
 \@startsection{paragraph}{3}{\parindent}{6pt plus 
1pt}{-5pt}{\reset@font\normalsize\itshape}}

\renewcommand{\@seccntformat}[1]{\textup{\csname the#1\endcsname}}

\gdef\@period{.}
\def\@trivlist{\@topsepadd\topsep
\if@noskipsec \gdef\@period{}\leavevmode\gdef\@period{.}\fi
 \ifvmode \advance\@topsepadd\partopsep \else \unskip\par\fi
 \if@inlabel \@noparitemtrue \@noparlisttrue 
 \else \@noparlistfalse \@topsep\@topsepadd \fi
 \advance\@topsep \parskip
 \leftskip\z@\rightskip\@rightskip \parfillskip\@flushglue
 \@setpar{\if@newlist\else{\@@par}\fi} \global\@newlisttrue
\@outerparskip\parskip}


\def\@startsection#1#2#3#4#5#6{%
  \if@noskipsec \leavevmode \fi
  \par
  \@tempskipa #4\relax
  \@afterindenttrue
  \ifdim \@tempskipa <\z@
    \@tempskipa -\@tempskipa \@afterindentfalse
  \fi
  \if@nobreak
    \everypar{}%
  \else
    \addpenalty\@secpenalty\addvspace\@tempskipa
  \fi
  \@ifstar
    {\@ssect{#3}{#4}{#5}{#6}}%
    {\@dblarg{\@sect{#1}{#2}{#3}{#4}{#5}{#6}}}}
\def\@sect#1#2#3#4#5#6[#7]#8{%
  \ifnum #2>\c@secnumdepth
    \let\@svsec\@empty
  \else
    \refstepcounter{#1}%
      \if@uchead%
            \protected@edef\@svsec{\@seccntformat{#1}\sepb\quad\relax}%
        \else%
            \protected@edef\@svsec{\@seccntformat{#1}\sepb\quad\relax}%
        \fi%
  \fi
  \@tempskipa #5\relax
  \ifdim \@tempskipa>\z@
    \begingroup
      #6{%
        \@hangfrom{\hskip #3\relax\@svsec}%
          \interlinepenalty \@M \if@uchead\MakeUppercase{#8}\else#8\fi \@@par}%
    \endgroup
    \csname #1mark\endcsname{#7}%
    \addcontentsline{toc}{#1}{%
      \ifnum #2>\c@secnumdepth \else
        \protect\numberline{\csname the#1\endcsname}%
      \fi
      #7}%
  \else
    \def\@svsechd{%
      #6{\hskip #3\relax
      \@svsec \if@uchead\Makeuppercase{#8}\else#8\fi}%
      \csname #1mark\endcsname{#7}%
      \addcontentsline{toc}{#1}{%
        \ifnum #2>\c@secnumdepth \else
          \protect\numberline{\csname the#1\endcsname}%
        \fi
        #7}}%
  \fi
  \@xsect{#5}}

\def\@xsect#1{\@tempskipa #1\relax
 \ifdim \@tempskipa>\z@
 \par \nobreak
 \vskip \@tempskipa
 \@afterheading
 \else \global\@nobreakfalse \global\@noskipsectrue
 \everypar{\if@noskipsec \global\@noskipsecfalse
 \clubpenalty\@M \hskip -\parindent
 \begingroup \@svsechd\@period \endgroup \unskip
 \hskip -#1
 \else \clubpenalty \@clubpenalty
 \everypar{}\fi}\fi\ignorespaces}
\newif\if@uchead\@ucheadfalse


\setcounter{secnumdepth}{3}
\newcounter{secnumbookdepth}
\setcounter{secnumbookdepth}{3}

\newfont{\apbf}{cmbx9}

\def\@withappendix#1{App--\number #1}

\newcommand{\elecappendix}{
}

\def\appenheader{\global\@topnum\z@ \global\@botroom \textheight \begin{figure}
\newfont{\sc}{cmcsc10}
\parindent\z@
\hbox{}
\vskip -\textfloatsep
\vskip 11pt
\hrule height .2pt width 30pc
\vskip 2pt\rule{0pt}{10pt}\ignorespaces}
\def\endappenheader{\end{figure}\gdef\appendixhead{}}

\def\@appsec{}

\def\appendix{\par
 \setcounter{section}{0}
 \setcounter{subsection}{0}
 \def\@appsec{APPENDIX } 
        \def\thesection{\Alph{section}}
        \def\theHsection{\Alph{section}}}



\labelsep 5pt
\settowidth{\leftmargini}{(9)} \addtolength\leftmargini\labelsep
\settowidth{\leftmarginii}{(b)} \addtolength\leftmarginii\labelsep
\leftmarginiii \leftmarginii
\leftmarginiv \leftmarginii 
\leftmarginv \leftmarginii 
\leftmarginvi \leftmarginii 
\leftmargin\leftmargini
\labelwidth\leftmargini\advance\labelwidth-\labelsep
\def\@listI{\leftmargin\leftmargini \parsep 0pt plus 1pt\topsep 6pt plus 2pt
minus 2pt\itemsep 2pt plus 1pt minus .5pt}
\let\@listi\@listI
\@listi 
\def\@listii{\leftmargin\leftmarginii
 \labelwidth\leftmarginii\advance\labelwidth-\labelsep
 \topsep 0pt plus 1pt 
 \parsep 0pt plus .5pt 
 \itemsep \parsep}
\def\@listiii{\leftmargin\leftmarginiii
 \labelwidth\leftmarginiii\advance\labelwidth-\labelsep
 \topsep 0pt plus 1pt 
 \parsep 0pt plus .5pt 
 \itemsep \parsep}
\def\@listiv{\leftmargin\leftmarginiv
 \labelwidth\leftmarginiv\advance\labelwidth-\labelsep}
\def\@listv{\leftmargin\leftmarginv
 \labelwidth\leftmarginv\advance\labelwidth-\labelsep}
\def\@listvi{\leftmargin\leftmarginvi
 \labelwidth\leftmarginvi\advance\labelwidth-\labelsep}




\def\enumerate{\ifnum \@enumdepth >3 \@toodeep\else
 \advance\@enumdepth \@ne 
 \edef\@enumctr{enum\romannumeral\the\@enumdepth}\list
 {\csname label\@enumctr\endcsname}{\usecounter
 {\@enumctr}\def\makelabel##1{##1\hss}}\fi}
\def\longenum{\ifnum \@enumdepth >3 \@toodeep\else
 \advance\@enumdepth \@ne 
 \edef\@enumctr{enum\romannumeral\the\@enumdepth}\list
 {\csname label\@enumctr\endcsname}{\usecounter
 {\@enumctr}\labelwidth\z@}\fi}
%\leftmargin\z@ \itemindent\parindent}\fi} - this indents each item in enumerate
\let\endlongenum\endlist
%%--------------------CHANGED: always roman parentheses. dave ---------------%%
\def\labelenumi{{\rm (}\arabic{enumi}\/{\rm )}} 
\def\theenumi{\arabic{enumi}} 
\def\labelenumii{{\rm (}\alph{enumii}\rm{)}}
\def\theenumii{\alph{enumii}}
\def\p@enumii{\theenumi}
\def\labelenumiii{\roman{enumiii}.}
\def\theenumiii{\roman{enumiii}}
\def\p@enumiii{\theenumi{\rm (}\theenumii{\rm )}}
\def\labelenumiv{\Alph{enumiv}.}
\def\theenumiv{\Alph{enumiv}} 
\def\p@enumiv{\p@enumiii\theenumiii}

\def\p@enumiv{\p@enumiii\theenumiii}

\def\itemize{\list{---\hskip -\labelsep}{\settowidth
 {\leftmargin}{---}\labelwidth\leftmargin
 \addtolength{\labelwidth}{-\labelsep}}}
\let\enditemize\endlist
\def\longitem{\list{---}{\labelwidth\z@
 \leftmargin\z@ \itemindent\parindent \advance\itemindent\labelsep}}
\let\endlongitem\endlist
\def\verse{\let\\=\@centercr 
 \list{}{\leftmargin 2pc 
 \itemindent -1.5em\listparindent \itemindent 
 \rightmargin\leftmargin\advance\leftmargin 1.5em}\item[]}
\let\endverse\endlist
\def\quotation{\list{}{\leftmargin 2pc \listparindent .5em
 \itemindent\listparindent
 \rightmargin\leftmargin \parsep 0pt plus 1pt}\item[]}
\let\endquotation=\endlist
\def\quote{\list{}{\leftmargin 2pc \rightmargin\leftmargin}\item[]}
\let\endquote=\endlist

\def\descriptionlabel#1{\hspace\labelsep {\bf #1}}
\def\description{\list{}{\labelwidth\z@ \itemindent-\leftmargin
       \let\makelabel\descriptionlabel}}

\let\enddescription\endlist

\def\describe#1{\list{}{\listparindent\parindent\settowidth{\labelwidth}{#1}\leftmargin
 \labelwidth\addtolength\leftmargin\labelsep\def\makelabel##1{##1\hfil}}}
\let\enddescribe\endlist

        \def\program{\ifx\@currsize\normalsize\small \else \rm \fi\tabbing}
        \let\endprogram\endtabbing
         \def\@begintheorem#1#2{\trivlist \item[\hskip 10pt\hskip 
         \labelsep{\sc{#1}\hskip 5pt\relax #2.}] \itshape}
        % aiellom{: this is what makes the theorem environment with names 
        % ABOVE #1 is the word example, corollary, etc.
        %            #2 is the number
        % \def\@opargbegintheorem#1#2#3{\trivlist
        % \item[\hskip 10pt \hskip \labelsep{\sc #1\savebox\@tempboxa{#3}\ifdim 
        % \wd\@tempboxa>\z@ \hskip 5pt\relax \box\@tempboxa\fi.}] \itshape}
        %  is been changed to
        % #1 is the word theorem, lemma, etc.
        % #2 is the number
        % #3 is the name of the theorem, lemma, etc.
        \def\@opargbegintheorem#1#2#3{\trivlist
        \item[\hskip 10pt \hskip 
\labelsep{\sc{#1}\savebox\@tempboxa{\sc{#3}}\ifdim 
        \wd\@tempboxa>\z@ \hskip 5pt\relax \sc{#2}  \box\@tempboxa\fi.}]
\itshape}
        % aiellom} 



\newif\if@qeded\global\@qededfalse
\def\proof{\global\@qededfalse\@ifnextchar[{\@xproof}{\@proof}}
\def\endproof{\if@qeded\else\qed\fi\endtrivlist}
\def\qed{\unskip\kern 10pt{\unitlength1pt\linethickness{.4pt}\framebox(6,6){}}
\global\@qededtrue}
\def\@proof{\trivlist \item[\hskip 10pt\hskip 
 \labelsep{\sc Proof.}]\ignorespaces}
\def\@xproof[#1]{\trivlist \item[\hskip 10pt\hskip 
 \labelsep{\sc Proof #1.}]\ignorespaces}

\def\newdef#1#2{\expandafter\@ifdefinable\csname #1\endcsname
{\@definecounter{#1}\expandafter\xdef\csname
the#1\endcsname{\@thmcounter{#1}}\global
 \@namedef{#1}{\@defthm{#1}{#2}}\global
 \@namedef{end#1}{\@endtheorem}}}
\def\@defthm#1#2{\refstepcounter
 {#1}\@ifnextchar[{\@ydefthm{#1}{#2}}{\@xdefthm{#1}{#2}}}
\def\@xdefthm#1#2{\@begindef{#2}{\csname the#1\endcsname}\ignorespaces}
\def\@ydefthm#1#2[#3]{\trivlist \item[\hskip 10pt\hskip 
 \labelsep{\it #2\savebox\@tempboxa{#3}\ifdim 
 \wd\@tempboxa>\z@ \ \box\@tempboxa\fi.}]\ignorespaces}
\def\@begindef#1#2{\trivlist \item[\hskip 10pt\hskip 
 \labelsep{\it #1\ \rm #2.}]}

\def\theequation{\arabic{equation}}

\def\titlepage{\@restonecolfalse\if@twocolumn\@restonecoltrue\onecolumn
 \else \newpage \fi \thispagestyle{empty}\c@page\z@}
\def\endtitlepage{\if@restonecol\twocolumn \else \newpage \fi}

\arraycolsep 2.5pt \tabcolsep 6pt \arrayrulewidth .4pt \doublerulesep 2pt 
\tabbingsep \labelsep 

\skip\@mpfootins = \skip\footins
\fboxsep = 3pt \fboxrule = .4pt 

\newcounter{part}
\newcounter{section}
\newcounter{subsection}[section]
\newcounter{subsubsection}[subsection]
\newcounter{paragraph}[subsubsection]

\def\thepart{\Roman{part}} 
\def\thesection {\arabic{section}}
\def\thesubsection {\arabic{section}.\arabic{subsection}}
\def\thesubsubsection {\arabic{section}.\arabic{subsection}.\arabic{subsubsection}}
\def\theparagraph {\arabic{section}.\arabic{subsection}.\arabic{subsubsection}.\arabic{paragraph}}

\def\@pnumwidth{1.55em}
\def\@tocrmarg {2.55em}
\def\@dotsep{4.5}
\setcounter{tocdepth}{3}

\def\tableofcontents{\section*{Contents\@mkboth{CONTENTS}{CONTENTS}}
 \@starttoc{toc}}
\def\l@part#1#2{\addpenalty{\@secpenalty}
 \addvspace{2.25em plus 1pt} \begingroup
 \@tempdima 3em \parindent \z@ \rightskip \@pnumwidth \parfillskip
-\@pnumwidth 
 {\large \bf \leavevmode #1\hfil \hbox to\@pnumwidth{\hss #2}}\par
 \nobreak \endgroup}
\def\l@section#1#2{\addpenalty{\@secpenalty} \addvspace{1.0em plus 1pt}
\@tempdima 1.5em \begingroup
 \parindent \z@ \rightskip \@pnumwidth 
 \parfillskip -\@pnumwidth 
 \bf \leavevmode #1\hfil \hbox to\@pnumwidth{\hss #2}\par
 \endgroup}
\def\l@subsection{\@dottedtocline{2}{1.5em}{2.3em}}
\def\l@subsubsection{\@dottedtocline{3}{3.8em}{3.2em}}
\def\listoffigures{\section*{List of Figures\@mkboth
 {LIST OF FIGURES}{LIST OF FIGURES}}\@starttoc{lof}}
\def\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}
\def\listoftables{\section*{List of Tables\@mkboth
 {LIST OF TABLES}{LIST OF TABLES}}\@starttoc{lot}}
\let\l@table\l@figure


\def\thebibliography#1{\par\footnotesize 
\@ucheadfalse
\@startsection{subsection}{2}{\z@}{16pt plus 2pt minus 
        1pt}{2pt}{\sf}*{REFERENCES}%
\list{\arabic{enumi}.}{%
 \settowidth{\labelwidth}{99.}%
 \leftmargin\labelwidth
 \advance\leftmargin\labelsep \topsep \z@ \parsep 0pt plus .1pt
 \itemsep \parsep 
 \usecounter{enumi}}%
 \def\newblock{\hskip .11em plus .33em minus .07em}
 \sloppy
 \widowpenalty=4500
 \clubpenalty=4500
 \sfcode`\.=1000\relax}
\let\endthebibliography=\endlist
 

\newif\if@restonecol
\def\theindex{\@restonecoltrue\if@twocolumn\@restonecolfalse\fi
\columnseprule \z@
\columnsep 35pt\twocolumn[\section*{Index}]
 \@mkboth{INDEX}{INDEX}\thispagestyle{plain}\parindent\z@
 \parskip\z@ plus .3pt\relax\let\item\@idxitem}
\def\@idxitem{\par\hangindent 40pt}
\def\subitem{\par\hangindent 40pt \hspace*{20pt}}
\def\subsubitem{\par\hangindent 40pt \hspace*{30pt}}
\def\endtheindex{\if@restonecol\onecolumn\else\clearpage\fi}
\def\indexspace{\par \vskip 10pt plus 5pt minus 3pt\relax}

\def\footnoterule{\kern-3\p@
 \hrule \@height 0.2\p@ \@width 47\p@ 
  \kern 2.6\p@
} 

\long\def\@makefntext#1{\parindent 1em\noindent 
 $^{\@thefnmark}$#1}


\setcounter{topnumber}{3}
\def\topfraction{.99}
\setcounter{bottomnumber}{1}
\def\bottomfraction{.5}
\setcounter{totalnumber}{3}
\def\textfraction{.01}
\def\floatpagefraction{.85}
\setcounter{dbltopnumber}{2}
\def\dbltopfraction{.7}
\def\dblfloatpagefraction{.5}

\long\def\@makecaption#1#2{\vskip 1pc \setbox\@tempboxa\hbox{#1.\hskip
1em\relax #2}
 \ifdim \wd\@tempboxa >\hsize #1. #2\par \else \hbox
to\hsize{\hfil\box\@tempboxa\hfil} 
 \fi}

\def\nocaption{\refstepcounter\@captype \par
 \vskip 1pc \hbox to\hsize{\hfil \footnotesize Figure \thefigure
 \hfil}}



\def\acmtable#1{\@narrowfig #1\relax
 \let\caption\@atcap \let\nocaption\@atnocap
 \def\@tmpnf{}\@ifnextchar[{\@xntab}{\@ntab}}
\def\endacmtable{\hbox to \textwidth{\hfil
\vbox{\hsize \@narrowfig
\box\@nfcapbox
{\baselineskip 4pt \hbox{\vrule height .4pt width \hsize}}
\vskip -1pt
\box\@nfigbox\vskip -1pt
{\baselineskip 4pt \hbox{\vrule height .4pt width \hsize}}}\hfil}
\end@float}
\def\@xntab[#1]{\def\@tmpnf{[#1]}\@ntab}
\def\@ntab{\expandafter\table\@tmpnf
 \setbox\@nfigbox\vbox\bgroup
 \hsize \@narrowfig \@parboxrestore}
\def\@atmakecap #1#2{\setbox\@tempboxa\hbox{#1.\hskip 1em\relax #2}
 \ifdim \wd\@tempboxa >\hsize \sloppy #1.\hskip 1em\relax #2 \par \else \hbox
to\hsize{\hfil\box\@tempboxa\hfil}
 \fi}
\def\@atcap{\par\egroup\refstepcounter\@captype 
 \@dblarg{\@atcapx\@captype}}
\long\def\@atcapx#1[#2]#3{\setbox\@nfcapbox\vbox {\hsize \wd\@nfigbox
 \@parboxrestore
 \@atmakecap{\csname fnum@#1\endcsname}{\ignorespaces #3}\par}}
\def\@atnocap{\egroup \refstepcounter\@captype 
 \setbox\@nfcapbox\vbox {\hsize \wd\@nfigbox
 \hbox to\hsize{\hfil \footnotesize Table \thetable\hfil}}}

%
%% Narrow figures
%
\def\narrowfig#1{\@narrowfig #1\relax
 \let\caption\@nfcap \let\nocaption\@nfnocap
 \def\@tmpnf{}\@ifnextchar[{\@xnfig}{\@nfig}}
\def\endnarrowfig{\hbox to \textwidth{\if@nfeven
 \box\@nfcapbox\hfil\box\@nfigbox
 \else \box\@nfigbox\hfil\box\@nfcapbox\fi}\end@float}
\def\@xnfig[#1]{\def\@tmpnf{[#1]}\@nfig}
\def\@nfig{\expandafter\figure\@tmpnf
 \setbox\@nfigbox\vbox\bgroup
 \hsize \@narrowfig \@parboxrestore}
\def\@nfmakecap #1#2{\setbox\@tempboxa\hbox{#1.\hskip 1em\relax #2}
 \ifdim \wd\@tempboxa >\hsize \sloppy #1.\hskip 1em\relax #2 \par \else \hbox
to\hsize{\if@nfeven\else\hfil\fi\box\@tempboxa\if@nfeven\hfil\fi}
 \fi}
\def\@nfcap{\par\egroup\refstepcounter\@captype 
 \@dblarg{\@nfcapx\@captype}}
\long\def\@nfcapx#1[#2]#3{\@seteven
 \setbox\@nfcapbox\vbox to \ht\@nfigbox
 {\hsize \textwidth \advance\hsize -2pc \advance\hsize -\wd\@nfigbox
 \@parboxrestore
 \vfil 
 \@nfmakecap{\csname fnum@#1\endcsname}{\ignorespaces #3}\par
 \vfil}}
\def\@nfnocap{\egroup \refstepcounter\@captype \@seteven
 \setbox\@nfcapbox\vbox to \ht\@nfigbox
 {\hsize \textwidth \advance\hsize -2pc \advance\hsize -\wd\@nfigbox
 \@parboxrestore
 \vfil 
 \hbox to\hsize{\if@nfeven\else\hfil\fi 
 \footnotesize Figure \thefigure
 \if@nfeven\hfil\fi}
 \vfil}}
\def\@seteven{\@nfeventrue 
   \@ifundefined{r@@nf\thefigure}{}{%
      \edef\@tmpnf{\csname r@@nf\thefigure\endcsname}%
      \edef\@tmpnf{\expandafter\@getpagenum\@tmpnf}%
      \ifodd\@tmpnf\relax\@nfevenfalse\fi}%
\label{@nf\thefigure}\edef\@tmpnfx{\if@nfeven e\else o\fi}
\edef\@tmpnf{\write\@unused {\noexpand\ifodd \noexpand\c@page 
 \noexpand\if \@tmpnfx e\noexpand\@nfmsg{\thefigure} \noexpand\fi
 \noexpand\else 
 \noexpand\if \@tmpnfx o\noexpand\@nfmsg{\thefigure}\noexpand\fi 
 \noexpand\fi }}\@tmpnf}
\def\@nfmsg#1{Bad narrowfig: Figure #1 on page \thepage}

\newdimen\@narrowfig
\newbox\@nfigbox
\newbox\@nfcapbox
\newif\if@nfeven 

\def\and{\\ and\\}
\def\maketitle{\newpage \thispagestyle{titlepage}\par
 \begingroup \lineskip = \z@\null \vskip -30pt\relax 
 \parindent\z@ \LARGE {\raggedright \hyphenpenalty\@M
 %\sf \@title \par
  \@title \par
 \global\firstfoot %aiellom
 \global\runningfoot %aiellom
}
\label{@firstpg}
{ \vskip 13.5pt\relax \normalsize \sf %vskip 13.5pt between title and author
 \begingroup \addtolength{\baselineskip}{2pt}
 \@author\par \vskip -2pt 
 \endgroup }
%% {\ifx \@categories\@empty 
%% \else 
%% \baselineskip 17pt\relax
  \hbox{\vrule height .2pt width 30pc}
%% \fi%to eliminate the lines for jacm
%% }
 \vskip 8.5pt \footnotesize \box\@abstract \vskip 4pt\relax %vskip8.5 space above abstract
%% {\def\and{\unskip\/{\rm ; }}
%% Categories and Subject Descriptors: \@categories 
%% \fi}
 \par\vskip 4pt\relax
%% \box\@terms \vskip 4pt\relax
 \box\@keywords \par 
 {\baselineskip 14pt\relax 
 \@abstractbottom
 }
 \vskip 23pt\relax
 \endgroup
\let\maketitle\relax
 % \gdef\@author{}\gdef\@title{}
 \gdef\@categories{}}


\newbox\@abstract
\newbox\@terms
\newbox\@keywords
\def\abstract{\global\setbox\@abstract=\vbox\bgroup \everypar{}
 \footnotesize \hsize 30pc \parindent 10pt \noindent 
 \rule{0pt}{10pt}\ignorespaces}
\def\endabstract{\egroup}

\def\terms#1{\setbox\@terms=\vbox{\everypar{}
 \footnotesize \hsize 30pc \parindent 0pt \noindent 
 General Terms: \ignorespaces #1}}
\def\keywords#1{\setbox\@keywords=\vbox{\everypar{}
 \footnotesize \hsize 30pc \parindent 0pt \noindent 
 Additional Key Words and Phrases: \ignorespaces #1}
}

\def\category#1#2#3{\@ifnextchar
 [{\@category{#1}{#2}{#3}}{\@xcategory{#1}{#2}{#3}}}
\def\@category#1#2#3[#4]{\edef\@tempa{\ifx \@categories\@empty 
 \else ; \fi}{\def\protect{\noexpand\protect
 \noexpand}\def\and{\noexpand\and}\xdef\@categories{\@categories\@tempa #1
[{\bf #2}]: 
 #3\kern\z@---\hskip\z@{\it #4}}}}
\def\@xcategory#1#2#3{\edef\@tempa{\ifx \@categories\@empty \else ;
\fi}{\def\protect{\noexpand\protect\noexpand}\def\and{\noexpand
 \and}\xdef\@categories{\@categories\@tempa #1 [{\bf #2}]: #3}}}
\def\@categories{}
\def\bottomstuff{\global\@topnum\z@ \global\@botroom \textheight \begin{figure}
\parindent\z@
\hbox{}
\vskip -\textfloatsep
\vskip 10pt
\hrule height .2pt width 30pc
\vskip 2pt\rule{0pt}{10pt}\ignorespaces}
\def\endbottomstuff{\permission\end{figure}\gdef\permission{}}

\newenvironment{ackslike}[1]
  {\par \footnotesize 
   \@ucheadfalse
   \@startsection{subsection}{2}{\z@}{-16pt plus -2pt minus -1pt}{2pt}{\sf}*
   {\uppercase{#1}}\par\normalsize
   \pdfbookmark{Acknowledgments}{Ack}
        }
  {\par}
\newenvironment{acks}{\begin{ackslike}{ACKNOWLEDGMENTS}}{\end{ackslike}}

\def\received{\par\footnotesize\addvspace{18pt plus 0pt minus
4pt}\parindent\z@}
%the line below replaced by the line that follows (K.R. Apt, July 11, 2000
%\def\endreceived{\label{@lastpg}\hskip-2pt\par}
\def\endreceived{\label{@lastpg}\hskip-2pt\par\normalsize}

\mark{{}{}} 

\def\ps@myheadings{\let\@mkboth\@gobbletwo
\def\@oddhead{\textcolor{grey-color}{\hbox{}{\it \mysubsubsection}\hfill \small\sf \mysubsection\hskip 19pt{$\blacksquare$}\hskip 17pt\mypage}}
\def\@oddfoot{\hbox{}\hfill\tiny\@runningfoot}
\def\@evenhead{\textcolor{grey-color}{\small\sf\mypage \hskip 17pt{$\blacksquare$}\hskip 19pt\mysection\hfill \hbox{}}}
\def\@evenfoot{\tiny\@runningfoot\hfill\hbox{}}
\def\sectionmark##1{}\def\subsectionmark##1{}}
\def\@runningfoot{}
\def\runningfoot{\def\@runningfoot{\parbox{0.95\textwidth}{\vskip10pt\textcolor{grey-color}{\theinfo\myauthor,\myaddress,\mydate}}}}
\def\@firstfoot{}
\def\firstfoot{\def\@firstfoot{\parbox{0.95\textwidth}{\vskip10pt\textcolor{grey-color}{\theinfo \myauthor, \myaddress, \mydate}}}}
\def\ps@titlepage{\let\@mkboth\@gobbletwo
\def\@oddhead{}\def\@oddfoot{\hbox{}\hfill
\tiny\@firstfoot}\def\@evenhead{}\def\@evenfoot{\tiny\@firstfoot\hfill\hbox{}}}

\def\today{\ifcase\month\or
 January\or February\or March\or April\or May\or June\or
 July\or August\or September\or October\or November\or December\fi
 \space\number\day, \number\year}
\def\@marrayclassiv{\@addtopreamble{$\displaystyle \@nextchar$}}
\def\@marrayclassz{\ifcase \@lastchclass \@acolampacol \or \@ampacol \or
 \or \or \@addamp \or
 \@acolampacol \or \@firstampfalse \@acol \fi
\edef\@preamble{\@preamble
 \ifcase \@chnum 
 \hfil$\relax\displaystyle\@sharp$\hfil \or $\relax\displaystyle\@sharp$\hfil 
 \or \hfil$\relax\displaystyle\@sharp$\fi}}
\def\marray{\arraycolsep 2.5pt\let\@acol\@arrayacol \let\@classz\@marrayclassz
 \let\@classiv\@marrayclassiv \let\\\@arraycr\def\@halignto{}\@tabarray}
\def\endmarray{\crcr\egroup\egroup}


\ps@myheadings \pagenumbering{arabic} \onecolumn 

%-----------------------BIBLIOGRAPHY STUFF-------------------------
% this is adapted (November 1993) by Andrew Appel and Rebecca Davies from
% 
%%%     filename        = "chicago.sty",
%%%     version         = "4",  % MODIFIED!
%%%     date            = "31 August 1992",
%%%     time            = "09:42:44 199",
%%%     author          = "Glenn Paulley",
%%%     address         = "Data Structuring Group
%%%                        Department of Computer Science
%%%                        University of Waterloo
%%%                        Waterloo, Ontario, Canada
%%%                        N2L 3G1",
%%%     telephone       = "(519) 885-1211",
%%%     FAX             = "(519) 885-1208",
%%%     email           = "gnpaulle@bluebox.uwaterloo.ca",

%%% ====================================================================
%
% this file: Modification of chicago.sty for new ACM bibliography
% style, which is similar (but not identical) to the ``Chicago'' style.
%
% chicago.sty: Style file for use with bibtex style chicago.bst, for
% bibliographies formatted according to the 13th Edition of the Chicago
% Manual of Style.
%
% 'newapa.bst' was made from 'plain.bst', 'named.bst', and 'apalike.bst',
% with lots of tweaking to make it look like APA style, along with tips
% from Young Ryu and Brian Reiser's modifications of 'apalike.bst'.
% newapa.sty formed the basis of this style, chicago.sty. Author-date
% references in newapa.bst formed the basis for chicago.bst. Chicagoa.bst
% supports annotations.
%
% Version 4 (August, 1992):
% - fixed chicago.bst and chicagoa.bst to handle long author lists in
%   sorting
% - fixed chicago.bst and chicagoa.bst so that missing page numbers in
%   ``article'' entries are handled correctly
% - modified chicago.sty to format entries with 2nd and subsequent lines
%   indented.
%
%   Citation format: (author-last-name year)
%             (author-last-name and author-last-name year)
%             (author-last-name et al. year)
%             (author-last-name)
%             author-last-name
%             author-last-name (year)
%             (author-last-name and author-last-name)
%             (author-last-name et al.)
%             (year) or (year,year)
%             year or year,year
%
%   Reference list ordering: alphabetical by author or whatever passes
%    for author in the absence of one.
%
% This BibTeX style has support for abbreviated author lists and for
%    year-only citations.  This is done by having the citations
%    actually look like
%
%    \citeauthoryear{full-author-info}{abbrev-author-info}{year}
%
% The LaTeX style has to have the following (or similar)
%
%     \let\@internalcite\cite
%     \def\fullcite{\def\citeauthoryear##1##2##3{##1, ##3}\@internalcite}
%     \def\fullciteA{\def\citeauthoryear##1##2##3{##1}\@internalcite}
%     \def\shortcite{\def\citeauthoryear##1##2##3{##2, ##3}\@internalcite}
%     \def\shortciteA{\def\citeauthoryear##1##2##3{##2}\@internalcite}
%     \def\citeyear{\def\citeauthoryear##1##2##3{##3}\@internalcite}
%
%
% -------------------------------------------------------------------------
%
% Citation macros.
%
\let\@internalcite\cite

\def\cite{\def\@citeseppen{-1000}%
    \def\@cite##1##2{[##1\if@tempswa , ##2\fi]}%
    \def\citeauthoryear##1##2##3{##2 ##3}\@internalcite}
\def\citeXNP#1#2{%
  \def\@cite@label{#2}%
  \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
  \def\citeauthoryear##1##2##3{\@cite@label}\@internalcite{#1}}
% \def\@citeseppen{-1000}%
%    \def\@cite@label{#1}
%    \def\@cite##1##2{\if@tempswa ##2\fi}%
%    \def\citeauthoryear##1##2##3{##2 ##3}\@internalcite}
\def\citeNP{\def\@citeseppen{-1000}%
    \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
    \def\citeauthoryear##1##2##3{##2 ##3}\@internalcite}
\def\citeN{\def\@citeseppen{-1000}%
    \def\@cite##1##2{##1\if@tempswa , ##2]\else{]}\fi}%
    \def\citeauthoryear##1##2##3{##2 [##3}\@citedata}
\def\shortcite#1{\citeyear{#1}}
\def\citeS#1{[\citeANP{#1} \citeyearNP{#1}]}
\def\citeNS#1{\citeANP{#1} \citeyear{#1}}
\def\citeNPS#1{\citeANP{#1} \citeyearNP{#1}}
%testing year,year
\def\citeNN{\def\@citeseppen{-1000}%
    \def\@cite##1##2{[##1\if@tempswa , ##2\fi]}%
    \def\citeauthoryear##1##2##3{##3}\@citedata}

\def\citeA{\def\@citeseppen{-1000}%
    \def\@cite##1##2{[##1\if@tempswa , ##2\fi]}%
    \def\citeauthoryear##1##2##3{##2}\@internalcite}
\def\citeANP{\def\@citeseppen{-1000}%
    \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
    \def\citeauthoryear##1##2##3{##2}\@internalcite}

\def\citeyear{\def\@citeseppen{-1000}%
    \def\@cite##1##2{[##1\if@tempswa , ##2\fi]}%
    \def\citeauthoryear##1##2##3{##3}\@citedata}
\def\citeyearNP{\def\@citeseppen{-1000}%
    \def\@cite##1##2{##1\if@tempswa , ##2\fi}%
    \def\citeauthoryear##1##2##3{##3}\@citedata}

%
% \@citedata and \@citedatax:
%
% Place commas in-between citations in the same \citeyear, \citeyearNP,
% or \citeN command.
% Use something like \citeN{ref1,ref2,ref3} and \citeN{ref4} for a list.
%
\def\@citedata{%
        \@ifnextchar [{\@tempswatrue\@citedatax}%
                                  {\@tempswafalse\@citedatax[]}%
}

\def\@citedatax[#1]#2{%
\if@filesw\immediate\write\@auxout{\string\citation{#2}}\fi%
  \def\@citea{}\@cite{\@for\@citeb:=#2\do%
    {\@citea\def\@citea{; }\@ifundefined% by Young
       {b@\@citeb}{{\bf ?}%
       \@warning{Citation `\@citeb' on page \thepage \space undefined}}%
{\csname b@\@citeb\endcsname}}}{#1}}%

% don't box citations, separate with ; and a space
% also, make the penalty between citations negative: a good place to break.
%
\def\@citex[#1]#2{%
\if@filesw\immediate\write\@auxout{\string\citation{#2}}\fi%
  \def\@citea{}\@cite{\@for\@citeb:=#2\do%
    {\@citea\def\@citea{; }\@ifundefined% by Young
       {b@\@citeb}{{\bf ?}%
       \@warning{Citation `\@citeb' on page \thepage \space undefined}}%
{\csname b@\@citeb\endcsname}}}{#1}}%

% (from apalike.sty)
% No labels in the bibliography.
%
\def\@biblabel#1{}

% (from apalike.sty)
% Set length of hanging indentation for bibliography entries.
%
\newlength{\bibhang}
\setlength{\bibhang}{2em}

% Indent second and subsequent lines of bibliographic entries. Stolen
% from openbib.sty: \newblock is set to {}.

\newdimen\bibindent
\bibindent=1em
\@ifundefined{refname}%
   {\@ifundefined{chapter}%
     {\newcommand{\refname}{\footnotesize REFERENCES}}%
     {\newcommand{\refname}{\footnotesize BIBLIOGRAPHY}}%
   }%
   {}%
\@ifundefined{chapter}%
 {\def\thebibliography#1{\footnotesize \section*{\refname\@mkboth 
   {\uppercase{\refname}}{\uppercase{\refname}}}\list
   {[\arabic{enumi}]}{
   \settowidth\labelwidth{[#1]}
   \leftmargin\labelwidth
   \advance\leftmargin\labelsep
   \advance\leftmargin\bibindent
   \itemindent -\bibindent
   \listparindent \itemindent
   \parsep \z@
   \usecounter{enumi}}
   \def\newblock{}
   \sloppy
   \sfcode`\.=1000\relax}}
 {\def\thebibliography#1{\footnotesize \chapter*{\refname\@mkboth 
   {\uppercase{\refname}}{\uppercase{\refname}}}\list
   {[\arabic{enumi}]}{\settowidth\labelwidth{[#1]}
   \leftmargin\labelwidth
   \advance\leftmargin\labelsep
   \advance\leftmargin\bibindent
   \itemindent -\bibindent
   \listparindent \itemindent
   \parsep \z@
   \usecounter{enumi}}
   \def\newblock{}
   \sloppy
   \sfcode`\.=1000\relax}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fixes to acm trans macro package
% 31-JAN-1996
% John Tang Boyland

% newdef need to take the optional parameters of newtheorem
\def\newdef#1{\@ifnextchar[{\@xnewdef{#1}}{\@ynewdef{#1}}}
\def\@xnewdef#1[#2]#3{\newtheorem{italic@#1}[#2]{{\em #3}}\@newdef{#1}}
\def\@ynewdef#1#2{\@ifnextchar[{\@xynewdef{#1}{#2}}{\@yynewdef{#1}{#2}}}
\def\@xynewdef#1#2[#3]{\newtheorem{italic@#1}{{\em #2}}[#3]\@newdef{#1}}
\def\@yynewdef#1#2{\newtheorem{italic@#1}{{\em #2}}\@newdef{#1}}
% and now fix up definition, to change body to use roman font:
\def\@newdef#1{\newenvironment{#1}{\@ifnextchar[{\@xstartdef{#1}}{\@ystartdef{#1}}}{\end{italic@#1}}}
%changed by K.R. Apt on Sep. 27, 2000. It was: \def\@xstartdef#1[#2]{\begin{italic@#1}[#2]\rm}
\def\@xstartdef#1[#2]{\begin{italic@#1}[{\em #2}]\rm}
\def\@ystartdef#1{\begin{italic@#1}\rm}

% footers produced too small (vpt), when ACM typsets them at 7.5pt (!).
% A better compromise is viipt.
\def\@oddfoot{\hbox{}\hfill\scriptsize\@runningfoot}
\def\@evenfoot{\scriptsize\@runningfoot\hfill\hbox{}}

% first footer was set to (vpt), when ACM typesets them at 6.5pt (!).
% A better compromise is vipt.
\def\firstfootsize{\@setsize\firstfootsize{7pt}\vipt\@vipt}
\def\ps@titlepage{\let\@mkboth\@gobbletwo
\def\@oddhead{}\def\@oddfoot{\hbox{}\hfill\firstfootsize\@firstfoot}%
\def\@evenhead{}\def\@evenfoot{\firstfootsize\@firstfoot\hfill\hbox{}}}

% new permission statement
\long\def\permission{\par
% linebreaks added to conform to the current style
Permission to make digital/hard copy of all or part of this material without
fee 
for personal or classroom use
provided that the copies are not made or distributed for profit
or commercial advantage, the ACM copyright/server notice, the title of the
publication, and its date appear, and notice is given that copying is by
permission of the ACM, Inc.  To copy
otherwise, to republish, to post on servers, or to redistribute to lists
requires prior specific 
permission and/or a fee.\par
\copyright\ 20\@acmYear\ 
ACM \@permissionCodeOne/20\@acmYear/\@permissionCodeTwo-\@pageCode\$5.00 % aiellom
}

% make sure \permission doesn't get trashed
\def\endbottomstuff{\permission\end{figure}}

% alternate way of using citeyear
%  \def\shortcite#1{\citeyear{#1}
% enumerations and longenums are wrong
% 1. the second line in a paragraph of an enumerations in theorems
%  (or other trivlists) doesn't line up under the first
% 1. the second line of a longenum doesn't properly wrap around
%  back to the left margin.

% the original definition of \@listI doesn't set labelwidth
\def\@listI{\leftmargin\leftmargini
 \labelwidth\leftmargini\advance\labelwidth-\labelsep
 \parsep 0pt plus 1pt
 \topsep 6pt plus 2pt minus 2pt
 \itemsep 2pt plus 1pt minus .5pt}
\let\@listi\@listI
\@listi 

% add set of \labelwidth and \itemindent
\def\longenum{\ifnum \@enumdepth >3 \@toodeep\else
 \advance\@enumdepth \@ne 
 \edef\@enumctr{enum\romannumeral\the\@enumdepth}\list
 {\csname label\@enumctr\endcsname}{\usecounter
 {\@enumctr}\labelwidth\z@\leftmargin\z@
 \itemindent\parindent \advance\itemindent\labelsep}\fi}

% split electronic appendix into two parts:


%aiellom{
\def\appendixhead#1{\appendix
\section*{ELECTRONIC APPENDIX}
The electronic appendix for this article can be accessed in
  the ACM Digital Library by visiting the following URL:$\;$ 
  \url{http://www.acm.org/pubs/citations/journals/}{\tt \@journalNameShort /20\@acmYear-\@acmVolume-\@acmNumber/p\pageref{@firstpg}-#1}.
% \quad \href{http://www.acm.org/tocl}{{\small http://www.acm.org/tocl} }
}
%aiellom}

\long\def\elecappendix{
\clearpage
\makeatletter
\pagenumbering{withappendix}
\makeatother
\appendix
\par\noindent{\sc This document is the online-only appendix to:}\hfill \vskip 1em
{\vbox{\Large\sf \parindent0pt\@title{}}}\vskip .5em
\vbox{\sf \parindent0pt\@author{}}\vskip .5em\noindent
\vskip 10pt\noindent
{\firstfootsize 
\@journalName, Vol.\ \@acmVolume, No.\ \@acmNumber, \@acmMonth\ 20\@acmYear, Pages \pages.}
\vskip 11pt\noindent
\hrule height .2pt
\par
\bottomstuff
\endbottomstuff
}

% provide for a single acknowledgment
\def\ack{ \par \footnotesize 
\@ucheadfalse
\@startsection{subsection}{2}{\z@}{-16pt plus -2pt minus 
 -1pt}{2pt}{\sf}*{ACKNOWLEDGMENT}\par\normalsize
\pdfbookmark{Acknowledgment}{Ack}
}
\def\endack{\par}

% provide both spellings of Acknowledgment(s)
\let\acknowledgments\acks
\let\endacknowledgments\endacks
\let\acknowledgment\ack
\let\endacknowledgment\endack

\newcommand{\longpage}{\enlargethispage{\baselineskip}}
\newcommand{\shortpage}{\enlargethispage{-\baselineskip}}


% Don't indent bibliography and
% override \refname (which sets fontsize):
\def\thebibliography#1{
  \footnotesize\section*{\footnotesize\sf{REFERENCES}\@mkboth
  {\uppercase{\refname}}{\uppercase{\refname}}}%
   \pdfbookmark{References}{Ref}
   \list
  {\@biblabel{\arabic{enumiv}}}{%
   \settowidth\labelwidth{[#1]}
   \labelwidth 0pt
   \leftmargin 2.0\bibindent
   \labelsep 0pt
   \if@hyperref
     \itemindent \bibindent
   \fi
   \listparindent 0pt
   \labelsep \bibindent
    \usecounter{enumiv}%
    \let\p@enumiv\@empty
    \def\theenumiv{\arabic{enumiv}}}%
    \def\newblock{\hskip .11em plus.33em minus.07em}%
    \sloppy
    \clubpenalty=4500
    \widowpenalty=4500
    \frenchspacing}
\def\endthebibliography{%
  \def\@noitemerr{\@warning{Empty `thebibliography' environment}}%
  \endlist}
\def\@lbibitem[#1]#2{\item[]\if@filesw
      {\def\protect##1{\string ##1\space}\immediate
       \write\@auxout{\string\bibcite{#2}{#1}}}\fi\hskip-1em\ignorespaces}
\def\ibibitem#1{\item{\hskip-10pt$[$\arabic{enumiv}$]$}}
\def\sbibitem#1{\ibibitem{#1}\if@filesw
      {\def\protect##1{\string ##1\space}\immediate
       \write\@auxout{\string\bibcite{#1}{\theenumiv}}}\fi}

% Fix cite so it doesn't repeat author lists in citations:

\def\cite{\def\@citeseppen{-1000}%
    \def\@cite##1##2{[##1\if@tempswa , ##2\fi]}%
    \let\@lastauthor=\@noauthor
    \let\citeauthoryear=\citeauthoryear@no@repeats\@internalcite}
    %\def\citeauthoryear##1##2##3{##2 ##3}\@internalcite

\def\@noauthor={\relax}
\let\@lastauthor=\@noauthor
\let\@currauthor=\@noauthor

\def\citeauthoryear@no@repeats#1#2#3{%
  \def\@currauthor{\csname @author #1\endcsname}%
  \ifx\@lastauthor\@currauthor{#3}\else{#2 #3}\fi%
  \let\@lastauthor=\@currauthor}

\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}


\if@hyperref
   \let\pdfbookmark=\relax
   \RequirePackage[pdfmark]{hyperref}
\fi

% \renewcommand{\thefootnote}{\arabic{footnote}\hspace{-3pt}}


