\version "2.19"

Lyrics = \lyricmode {
  AL -- LE -- LU -- IA.
Lau -- da -- te Do -- mi -- num
in sanc -- tis e -- ius.
Lau -- da -- te e -- um 
in fir -- ma -- men -- to 
vir -- tu -- tis e -- ius.
}

MusicS = { bes'1  bes'2  a'1  b'2  a'2  bes'2  bes'2  a'1  a'2  b'1.  d''2  ees''2  d'2  c'1  b2  cis''r1  d''2  ees''1  d'2  c'1.  b1.  b2  c'2  c'2  b1  c'2  d''2  ees''2  ees''2  d''1  d''2   \bar "|." }

MusicA = { g'1  g'2  fis'1  g'2  fis'2  g'2  g'2  fis'1  fis'2  g'1.  g'2  g'2  g'2  g'1  g'2  cis''r1  g'2  g'1  g'2  g'1.  g'1.  g'2  g'2  g'2  g'1  g'2  fis'2  g'2  g'2  g'1  g'2   \bar "|." }

MusicT = { d'1  d'2  d'1  d'2  d'2  d'2  d'2  d'1  d'2  d'1.  b,2  cis2  d2  ees1  d2  cis''r1  b,2  cis1  d2  ees1.  d1.  d'2  ees'2  ees'2  d'1  e'2  b2  b,2  c2  b,1  c2   \bar "|." }

MusicB = { g1  g2  d1  g2  d2  b,2  g,2  d1  d2  g,1.  g,2  c,2  b,,2  c,1  g,2  cis''r1  g,2  c,1  b,,2  c,1.  g,1.  g2  e,2  c,2  g,1  c,2  b,2  e,2  c,2  g,1  c,2   \bar "|." }

\score {
  <<
    \new ChoirStaff
    <<
      \new Staff
      <<
        \new Voice = "S" {
           \clef "treble"
              \time 3/2
              \key c\major
              \MusicS
        }
        \new Lyrics \lyricsto "S" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "A" {
           \clef "treble"
              \time 3/2
              \key c\major
              \MusicA
        }
        \new Lyrics \lyricsto "A" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "T" {
           \clef "treble_8"
              \time 3/2
              \key c\major
              \MusicT
        }
        \new Lyrics \lyricsto "T" { \Lyrics }
      >>
      \new Staff
      <<
        \new Voice = "B" {
           \clef "bass"
              \time 3/2
              \key c\major
              \MusicB
        }
        \new Lyrics \lyricsto "B" { \Lyrics }
      >>
    >>
  >>
}
