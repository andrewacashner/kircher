\version "2.19"

Lyrics = \lyricmode {
  AL -- LE -- LU -- IA.
Lau -- da -- te Do -- mi -- num
in sanc -- tis e -- ius.
Lau -- da -- te e -- um 
in fir -- ma -- men -- to 
vir -- tu -- tis e -- ius.
}

MusicS = { d''2  d''2  d''2  d''2  d''1  c''2  bes'2  a'1.  a'2  bes'1  d''1  ees''2.  d''4  d''2  d''2  r4  d''4  ees''4  d''4  c''2  bes'2  d''2  ees''1  d''2  c''1  bes'1  bes'2  c''4  bes'4  a'2  bes'2   \bar "|." }

MusicA = { fis'2  g'2  fis'2  g'2  f'1  e'2  g'2  g'1.  fis'2  g'1  g'1  g'2.  g'4  fis'2  g'2  r4  g'4  g'4  g'4  g'2  g'2  g'2  g'1  g'2  g'1  g'1  g'2  g'4  g'4  fis'2  g'2   \bar "|." }

MusicT = { a2  bes2  a2  bes2  bes1  a2  d'2  ees'1.  d'2  d'1  bes1  c'2.  bes4  a2  bes2  r4  bes4  c'4  d'4  ees'2  d'2  bes2  c'1  d'2  ees'1  d'1  d'2  ees'4  d'4  d'2  d'2   \bar "|." }

MusicB = { d2  g2  d2  g,2  d1  a,2  bes,2  c1.  d2  g,1  g,1  c2.  d4  d2  g,2  r4  g4  c'4  bes4  c'2  g2  g2  c'1  bes2  c'1  g1  g2  c4  g4  d2  g,2   \bar "|." }

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
