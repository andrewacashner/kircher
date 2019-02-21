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
r2 g1 g2 g1 g1 
g4 f8 e8 d4. d8 e1 
e2 f1 e2 d1 a1 
d2 e4 e4 d2 a2 
d2 e4 e4 d2 a2 
e4 f8 e8 d4 a4 
 \bar "|." }

MusicA = {
r2 bes1 a2 bes1 a1 
a4 a8 a8 bes4. bes8 a1 
a2 a1 a2 a1 a1 
bes2 a4 a4 bes2 a2 
bes2 a4 a4 bes2 a2 
a4 a8 a8 a4 a4 
 \bar "|." }

MusicT = {
r2 d1 e2 d1 e1 
e4 a8 g8 g4. g8 g1 
g2 a1 g2 g1 a1 
g2 g4 g4 g2 a2 
g2 g4 g4 g2 a2 
g4 a8 g8 g4 a4 
 \bar "|." }

MusicB = {
r2 g1 a2 g1 a1 
a4 f8 a8 g4. g8 a1 
a2 f1 a2 g1 a1 
g2 e4 a4 g2 a2 
g2 e4 a4 g2 a2 
a4 f8 a8 g4 a4 
 \bar "|." }

\score {
  <<
    \new ChoirStaff
    <<
      \new Staff
      <<
        \new Voice = "S" {
           \clef "treble"
              \time 4/2
              \key c\major
              \MusicS
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
              \time 4/2
              \key c\major
              \MusicA
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
              \time 4/2
              \key c\major
              \MusicT
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
              \time 4/2
              \key c\major
              \MusicB
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
