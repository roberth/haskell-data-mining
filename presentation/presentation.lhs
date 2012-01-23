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
\newcommand{\To}{$\rightarrow$~}

\frame{\titlepage}

\section{Introduction}
\begin{frame}{Introduction}
  \tableofcontents
\end{frame}

\section{Decision Trees}

\begin{frame}{Decision Trees}
\subsection{Defining classification}

\begin{center}A classification method\end{center}

\end{frame}

\begin{frame}{Classification}

Example: Iris plant data

\begin{block}{Database}
> data IrisClass = Setosa | Versicolour | Virginica
> data Iris = Iris { 
>  sepalLength :: Double,
>  sepalWidth :: Double,
>  petalLength :: Double,
>  petalWidth :: Double,
>  irisClass :: IrisClass
> }
> db :: [Iris]
\end{block}

New iris data record: given dimensions, which class to assign?

\end{frame}

\begin{frame}{Decision Trees}
Data
\begin{description}
\item[record] attributes and class label together
\item[attributes] a combination of Int, Double, Bool, ...
\item[class label] usually something simple like Bool or an ADT with a few nullary constructors
\end{description}

Tasks
\begin{itemize}
\item Classification: model \To attributes \To label

\item Supervised learning: [record] \To model
\end{itemize}
\end{frame}

\begin{frame}{Learning}

\begin{itemize}
\item Generated from the data
\item Recursively construct the tree, from root to leaf
\begin{itemize}
        \item Determine all ways to meaningfully split the database
        \item Determine split with highest quality
        \item Actually split the database
        \item Recurse for branches, \\where database := relevant partition of db
\end{itemize}
\item Put a leaf node when db has one label or can not be split
\end{itemize}

\end{frame}

\begin{frame}{Data Types}

Many types
\begin{itemize}
  \item Attribute \uncover<2->{-- no}
  \item Class label \uncover<2->{-- no}
  \item Separators \uncover<3->{-- yes, but ambiguous}
  \item Separator labels \uncover<3->{-- yes, depend on Separators}
  \item Decision tree \uncover<3->{-- yes, depend on all above}
\end{itemize}

Which to infer?

\end{frame}

\begin{frame}{Inferring Separation Criteria}

Splitting:
\begin{itemize}
  \item Equality, (Eq x) $\Rightarrow$ ($==$ x): Cat, Int
  \item Set membership, (Eq x) $\Rightarrow$ (`elem` [x]): \{Cat, Dog\}, Int
  \item Ordering (Ord x) $\Rightarrow$ ($<$ x): Int, Double
\end{itemize}
\begin{itemize}
  \item Tupling: (Double, Double)  -- just cartesian product
  \item Distance: (Double, Double) -- euclidian space
\end{itemize}

\end{frame}

\begin{frame}{The Rules}

> class GenSep attribute separator | attr -> separator
> instance GenSep Char (SepSet Char)
> instance GenSep Double (SepOrd Double)
> instance GenSep Float (SepOrd Float)
> instance GenSep Int (SepOrd Int)
> instance GenSep Integer (SepOrd Integer)
> instance GenSep [Char] (SepSet [Char])
> instance Integral a => GenSep (Ratio a) (SepOrd (Ratio a))
> instance (GenSep a xa, GenSep b xb) => GenSep (a, b) (Either xa xb)

\end{frame}

\begin{frame}{Example Tree}
> > buildDTree irisAttrs irisClass iris
> Node (Right (Left (SepOrd 2.45))) [(False,Node (Right (Right 
> (SepOrd 1.75))) [(False,Node (Right (Left (SepOrd 4.85))) [
> (False,Leaf Virginica),(True,Node (Left (Left (SepOrd 5.95)
> )) [(False,Leaf Virginica),(True,Leaf Versicolor)])]),(True,
> Node (Right (Left (SepOrd 4.95))) [(False,Node (Right (Right
> (SepOrd 1.55))) [(False,Node (Left (Left (SepOrd 6.95))) [(
> False,Leaf Virginica),(True,Leaf Versicolor)]),(True,Leaf
> Virginica)]),(True,Node (Right (Right (SepOrd 1.65))) [(
> False,Leaf Virginica),(True,Leaf Versicolor)])])]),(True,
> Leaf Setosa)]

\end{frame}

\begin{frame}{Example Tree}
> > buildDTree irisAttrs irisClass iris
\includegraphics[width=\textwidth]{iristree.pdf}
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
>  type Transaction a = Set a
>  type Items a = Set a
>  rules :: Item a => [Transaction a] -> Items a -> 
>    (Map (Items a) Int -> Map (Items a) Int) -> 
>    Map (Items a, Items a) Double
\pause
>  {-"\uncover<3->{\{"-}-# SPECIALIZE frequencyBy ::
>    (Items Int -> Items Int -> Bool) ->
>    [Items Int] -> [Transaction Int] -> [(Items Int, Int)] #-{-"\}}"-}
>  frequencyBy :: {-"\uncover<4->{"-}(NFData a) => {-"}"-} 
>    (a -> b -> Bool) -> [a] -> [b] -> [(a,Int)]
>  frequencyBy f as bs = 
>    map (\ a ->(a, foldr (\ b -> if f a b then (+) 1 else id) 0 bs)) as
>      {-"\uncover<4->{"-}`using` parListChunk 100 rdeepseq{-"}"-}
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

\section{Conclusion}
\begin{frame}{Conclusion}
  What did we learn:
  \begin{itemize}
    \item Parallelising in Haskell
    \item Optimizing in Haskell
    \item Designing an easy to use interface for an algorithm in Haskell
  \end{itemize}
  What did we contribute:
  \begin{itemize}
    \item Implementations of algorithms
    \item A lean interface for other algorithm implementations to crystallize on. Hopefully the beginning of a modular data analysis/learning framework that promotes reuse and compatibility
  \end{itemize}
\end{frame}

\end{document}
