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
bes4 a4 d1 g2 
a4 bes8 bes8 a4. a8 bes1 
c2 bes1 a2 d1 d1 
d2 es4 d4 c2 d2 
r4 a4 bes4 bes4 a2 d2 
d4 es2 es4 d1 d1 
 \bar "|." }

MusicA = {
g4 es4 d1 d2 
d4 g8 g8 d4. d8 g1 
a2 g1 es2 d1 d1 
g2 g4 g4 g2 d2 
r4 d4 g4 g4 d2 d2 
d4 g2 g4 g1 d1 
 \bar "|." }

MusicT = {
d4 c4 a1 bes2 
d4 d8 d8 d4. d8 d1 
es2 d1 c2 a1 d1 
bes2 c4 d4 es2 d2 
r4 d4 d4 d4 d2 d2 
bes4 bes2 c4 bes1 d1 
 \bar "|." }

MusicB = {
bes4 c4 d1 g2 
d4 bes8 g8 d4. d8 g1 
a2 bes1 c2 d1 d1 
g2 c4 bes4 c2 d2 
r4 d4 bes4 g4 d2 d2 
bes4 es2 c4 g1 d1 
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
              \key f\major
              \MusicS
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
              \time 4/2
              \key f\major
              \MusicA
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
              \time 4/2
              \key f\major
              \MusicT
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
              \time 4/2
              \key f\major
              \MusicB
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
