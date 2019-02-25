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
r4 a'2 a'4 a'2 a'2 
e'2 f'1 f'2 e'2. e'4 f'1 
e'2 f'4 f'4 e'2 f'2 
f'2 g'4 f'4 e'2 f'2 
a'1 bes'2 bes'2 a'1 a'1 
f'4 g'8 g'8 f'4 g'4 
 \bar "|." }

MusicA = {
r4 cis2 d4 cis2 d2 
cis2 d1 d2 cis2. cis4 d1 
cis2 d4 d4 cis2 d2 
d2 d4 d4 d2 d2 
cis1 d2 d2 d1 d1 
d4 d8 d8 d4 d4 
 \bar "|." }

MusicT = {
r4 e2 f4 e2 f2 
a2 a1 a2 a2. a4 a1 
a2 a4 a4 a2 a2 
a2 bes4 a4 a2 a2 
f1 f2 g2 f1 g1 
a4 bes8 bes8 a4 bes4 
 \bar "|." }

MusicB = {
r4 a,2 d,4 a,2 d,2 
a,2 f,1 d,2 a,2. a,4 d,1 
a,2 f,4 d,4 a,2 d,2 
d,2 g,4 d,4 a,2 d,2 
f,1 bes,2 g,2 d,1 g,1 
d,4 bes,8 g,8 d,4 g,4 
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
