\version "2.19"

Lyrics = \lyricmode {
  AL -- LE -- LU -- IA.
Lau -- da -- te Do -- mi -- num
in sanc -- tis e -- ius.
Lau -- da -- te e -- um 
in fir -- ma -- men -- to 
vir -- tu -- tis e -- ius.
}

MusicS = {
a'1 g'2 e''1 f''2 
c''2 d''2 c''2 c''2. c''4 c''2 
r1 g'2 a'1 a'2 g'1. a'1. 
r1 c''2 d''1 c''2 bes'1. a'1. 
bes'2 a'2 g'2 e''1 f''2 
r1 a'2 bes'1 bes'2 a'1. bes'1. 
 \bar "|." }

MusicA = {
f'1 d'2 c'1 c'2 
f'2 f'2 f'2 e'2. e'4 f'2 
r1 e'2 f'1 f'2 e'1. f'1. 
r1 f'2 f'1 f'2 f'1. f'1. 
g2 f'2 d'2 c'1 c'2 
r1 f'2 f'1 f'2 f'1. f'1. 
 \bar "|." }

MusicT = {
c'1 bes2 g1 a2 
a2 bes2 a2 g2. g4 a2 
r1 c'2 c'1 c'2 c'1. c'1. 
r1 a2 bes1 c'2 d'1. c'1. 
d'2 c'2 bes2 g1 a2 
r1 c'2 d'1 d'2 c'1. d'1. 
 \bar "|." }

MusicB = {
a1 bes2 c'1 f2 
f2 bes,2 f2 c2. c4 f,2 
r1 c'2 a1 f2 c'1. f1. 
r1 f2 bes,1 a,2 bes,1. f,1. 
g2 a2 bes2 c'1 f2 
r1 f2 d1 bes,2 f,1. bes,1. 
 \bar "|." }

\score {
  <<
    \new ChoirStaff
    <<
      \new Staff
      <<
        \new Voice = "S" {
           \clef "treble"
              \time 3/2
              \key f\major
              \MusicS
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
              \time 3/2
              \key f\major
              \MusicA
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
              \time 3/2
              \key f\major
              \MusicT
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
              \time 3/2
              \key f\major
              \MusicB
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
