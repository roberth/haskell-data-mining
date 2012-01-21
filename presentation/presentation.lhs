\documentclass{beamer}

\def\textmu{}

%include polycode.fmt
\usepackage{color}
\usepackage{graphicx}
\definecolor{ogreen}{RGB}{0,76,0}
\definecolor{oorange}{RGB}{255,127,0}
%format \ = "\lambda"
%formal using = \text{using}

\usepackage[utf8x]{inputenc}
\usepackage{amsmath,amsthm,amssymb,stmaryrd}

\usetheme{Madrid}
\setbeamertemplate{navigation symbols}{} 
\title[Data Mining]{Data Mining in Haskell}
\institute[Utrecht University]{Department of Information and Computing
  Sciences\\Faculty of Science, Utrecht University}

\author[]{Robert Hensing \& Hidde Verstoep}

\date{}

\begin{document}

\frame{\titlepage}

\section{Introducation}
\begin{frame}{Introduction}
  \tableofcontents
\end{frame}

\section{Apriori}
\subsection{How does Apriori work?}
\begin{frame}{Apriori (1)}
  \begin{center}\includegraphics[scale=0.5]{apriori-2}\end{center}
\end{frame}

\begin{frame}{Apriori (2)}
  \begin{center}\includegraphics[scale=0.5]{apriori-3}\end{center}
\end{frame}

\subsection{Interesting implementation details}
\subsubsection{Specialization}
\subsubsection{Parallelization}
\begin{frame}{Apriori (3)}
>  {-"\uncover<2->{\{"-}-# SPECIALIZE frequencyBy ::
>    (Set Int -> Set Int -> Bool) ->
>    [Set Int] -> [Set Int] -> [(Set Int, Int)] #-{-"\}}"-}
>  frequencyBy :: {-"\uncover<3->{"-}(NFData a) => {-"}"-} 
>    (a -> b -> Bool) -> [a] -> [b] -> [(a,Int)]
>  frequencyBy f as bs = 
>    map (\ a ->(a, foldr (\ b -> if f a b then (+) 1 else id) 0 bs)) as
>      {-"\uncover<3->{"-}`using` parListChunk 100 rdeepseq{-"}"-}
\end{frame}

\begin{frame}{Parallelization}
>  import Control.Parallel.Strategies
>  
>  using :: a -> Strategy a -> a
>  
>  r0 :: Strategy a
>  rseq :: Strategy a
>  rdeepseq :: NFData a => Strategy a
> 
>  parList :: Strategy a -> Strategy [a]
>  parListChunk :: Int -> Strategy a -> Strategy [a]
\end{frame}

\section{Decision Trees}
\begin{frame}
\end{frame}

\section{Naive Bayes}
\begin{frame}
\end{frame}

\section{conclusion}
\begin{frame}{Conclusion}
\end{frame}

\end{document}
