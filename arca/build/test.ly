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
r2 g1 a2 bes1 a1 
a2 bes1 a2 a2. a4 a1 
a4 bes8 a8 g4 f4 
e4 d2 e4 cis1 d1 
a4 bes2 bes4 a1 a1 
f1 g2. f4 e2 f2 
 \bar "|." }

MusicA = {
r2 d1 d2 d1 d1 
d2 d1 d2 cis2. cis4 d1 
d4 d8 d8 d4 d4 
bes4 a2 bes4 a1 a1 
cis4 d2 d4 d1 d1 
d1 d2. d4 d2 d2 
 \bar "|." }

MusicT = {
r2 bes1 a2 g1 f1 
f2 g1 f2 e2. e4 f1 
f4 g8 a8 bes4 a4 
g4 f2 e4 e1 f1 
f4 f2 g4 f1 g1 
a1 bes2. a4 a2 a2 
 \bar "|." }

MusicB = {
r2 g1 f2 g1 d1 
d2 g1 d2 a2. a4 d1 
d4 g8 f8 g4 d4 
e4 f2 g4 a1 d1 
f4 bes2 g4 d1 g1 
d1 g2. d4 a2 d2 
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
