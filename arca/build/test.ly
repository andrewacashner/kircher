\version "2.19"

Lyrics = \lyricmode {
  AL -- LE -- LU -- IA.
Lau -- da -- te Do -- mi -- num
in sanc -- tis e -- ius.
Lau -- da -- te e -- um 
in fir -- ma -- men -- to 
vir -- tu -- tis e -- ius.
}

MusicS = { g'1  a'1  bes'1  a'1  e''2  f''4  f''4  e''4.  e''8  f''2  a'2  bes'1  bes'2  a'1  a'1  e''1  f''2.  f''4  e''2  f''2  g'1  f'2.  e'4  cis'2  d'2  g'2  f'1  e'2  cis'1  d'1   \bar "|." }

MusicA = { d''1  d''1  d''1  d''1  cis'2  d'4  d'4  cis'4.  cis'8  d'2  cis'2  d'1  d'2  d'1  d'1  cis'1  d'2.  d'4  cis'2  d'2  ees'1  d'2.  bes'4  a'2  a'2  ees'2  d'1  bes'2  a'1  a'1   \bar "|." }

MusicT = { bes1  a1  g1  f1  a2  a4  a4  a4.  a8  a2  fis2  fis1  g2  fis1  g1  a1  a2.  a4  a2  a2  bes1  a2.  g4  e2  f2  bes2  a1  g2  e1  f1   \bar "|." }

MusicB = { g1  fis1  g1  d1  a2  f4  d4  a4.  a8  d2  f,2  b,1  g,2  d1  g,1  a1  f2.  d4  a2  d2  e1  fis2.  g4  a2  d2  e2  fis1  g2  a1  d1   \bar "|." }

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
