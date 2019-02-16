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
g'1 g'2 g'1 g'2 
g'2 f'2 e'2 d'1 d'2 e'1. 
g'2 a'2 g'2 f'1 e'2 
f'2 e'2 d'2 b'1 c''2 
r1 e'2 f'1 f'2 e'1. f'1. 
r1 g'2 a'1 a'2 g'1. g'1. 
}

MusicA = {
b'1 c''2 b'1 c''2 
b'2 a'2 c''2 c''1 b'2 c''1. 
c''2 c''2 c''2 c''1 c''2 
d'2 c''2 a'2 g'1 g'2 
r1 c''2 c''1 c''2 c''1. c''1. 
r1 b'2 c''1 c''2 c''1. c''1. 
}

MusicT = {
d1 e2 d1 e2 
e2 d2 g2 a1 g2 g1. 
e2 f2 g2 a1 g2 
a2 g2 f2 d1 e2 
r1 g2 a1 a2 g1. a1. 
r1 e2 e1 f2 e1. f1. 
}

MusicB = {
g1 c'2 g1 c2 
g2 d2 e2 f1 g2 c1. 
c'2 f2 e2 f1 c2 
d2 e2 f2 g1 c2 
r1 c'2 a1 f2 c1. f1. 
r1 e2 a1 f2 c'1. f1. 
}

\score {
  <<
    \new ChoirStaff
    <<
      \new Staff
      <<
        \new Voice = "S" {
           \clef "treble"
              \time 3/2
              \MusicS
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
              \time 3/2
              \MusicA
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
              \time 3/2
              \MusicT
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
              \time 3/2
              \MusicB
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
