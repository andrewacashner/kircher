\version "2.19"

Lyrics = \lyricmode {
  AL -- LE -- LU -- IA.
Lau -- da -- te Do -- mi -- num
in sanc -- tis e -- ius.
Lau -- da -- te e -- um 
in fir -- ma -- men -- to 
vir -- tu -- tis e -- ius.
}

MusicS = { r4  e2  f4  e2  f2  a'1  g'2  f'2  e'1.  e'2  f'1  e'4  f'2  f'4  e'1  f'1  fis'2  g'4  f'4  e'2  f'2  e'2  f'1  f'2  e'1  f'1  fis'1  g'2.  f'4  e'2  f'2   \bar "|." }

MusicA = { r4  cis,2  d4  cis2  d2  c1  b,2  d2  d1.  cis2  d1  cis4  d2  d4  cis1  d1  d'2  d'4  d'4  d'2  d'2  cis2  d1  d2  cis1  d1  d'1  d'2.  d'4  d'2  d'2   \bar "|." }

MusicT = { r4  a,2  a,4  a,2  a,2  f1  e2  a2  bes1.  a2  a1  a4  a2  a4  a1  a1  a2  bes4  a4  a2  a2  a2  a1  a2  a1  a1  a1  bes2.  a4  a2  a2   \bar "|." }

MusicB = { r4  a,,2  d,4  a,2  d,2  a,1  e,2  fis,2  g,1.  a,2  d,1  a,4  f,2  d,4  a,1  d,1  d2  g,4  d4  a,2  d,2  a,2  f,1  d,2  a,1  d,1  d1  g,2.  d4  a,2  d,2   \bar "|." }

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
