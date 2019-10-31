
\version "2.19"
\include "early-music.ly"
\include "automatic-ties.ly"

\score {
<<
  \new ChoirStaff
  <<
    
\new Staff
  <<
    \new Voice {
      \clef "treble"
      \time 4/2
      r2 a'1 b'2 a'1 a'1 |

    }
  >>
 
\new Staff
  <<
    \new Voice {
      \clef "treble"
      \time 4/2
      r2 d'1 d'2 c'1 d'1 |

    }
  >>
 
\new Staff
  <<
    \new Voice {
      \clef "treble_8"
      \time 4/2
      r2 f'1 g'2 e'1 f'1 |

    }
  >>
 
\new Staff
  <<
    \new Voice {
      \clef "bass"
      \time 4/2
      r2 d1 g2 a1 d1 |

    }
  >>

  >>
>>
}
 
\score {
<<
  \new ChoirStaff
  <<
    
\new Staff
  <<
    \new Voice {
      \clef "treble"
      \time 4/2
      b'2 b'4 a'4 b'4. b'8 a'2 |
 a'2 b'4 a'4 g'2 f'2 |
 a'2 g'1 f'2 e'2. e'4 f'1 |
 f'2 e'2 c''1 d''1 |
 e''2 f''1 f''2 e''1 f''1 |
 e''2 f''4 f''4 e''2 f''2 |
 g''1 e''1 d''1 |
 d''2 e''2 c''2. c''4 d''1 |
 c''1. d''2 e''1. e''2 e''1 |
 g''4. f''8 |
 e''1 e''2 e''2 e''2. e''4 e''1 |
 e''1 f''1 e''1 f''1 |
 d''4 g''8 a''8 b''4. b''8 a''1 |
 r2 c''1 d''2 c''1. c''2 d''1 |
 a'1. a'2 a'1 |
 a'4 b'8 a'8 a'4. a'8 a'1 |
 e''1 e''2 e''2 e''2. e''4 e''1 |
 e''1 f''2 f''2 e''1. e''2 f''1 |
 e''1 f''2 f''2 e''1 f''1 |
 r4 g''4 f''4. a'8 a'1 |
 r4 a'4 b'4 a'4 a'4. a'8 a'2 |
 f''4. f''2 g''1 |
 d''2 g''4 a''4 b''4. b''8 a''2 |
 c''1. d''2 c''1. c''2 d''1 |
 r2 c''1 d''2 c''1. c''2 d''1 |

    }
  >>
 
\new Staff
  <<
    \new Voice {
      \clef "treble"
      \time 4/2
      d'2 d'4 d'4 d'4. d'8 d'2 |
 d'2 d'4 d'4 d'2 d'2 |
 d'2 d'1 d'2 c'2. c'4 d'1 |
 d'2 b2 a1 a1 |
 c'2 d'1 d'2 c'1 d'1 |
 c'2 d'4 d'4 c'2 d'2 |
 d'1 c'1 b1 |
 b2 b2 a2. a4 a1 |
 a1. a2 b1. b2 c'1 |
 d'4. d'8 |
 c'1 b2 c'2 b2. b4 c'1 |
 c'1 d'1 c'1 d'1 |
 b4 d'8 d'8 d'4. d'8 d'1 |
 r2 a'1 a'2 a'1. a'2 a'1 |
 c'1. c'2 d'1 |
 d'4 d'8 d'8 c'4. c'8 d'1 |
 c'1 b2 c'2 b2. b4 c'1 |
 c'1 d'2 d'2 c'1. c'2 d'1 |
 c'1 d'2 d'2 c'1 d'1 |
 r4 b4 b4. c'8 d'1 |
 r4 d'4 d'4 d'4 c'4. c'8 d'2 |
 d'4. d'2 d'1 |
 b2 d'4 d'4 d'4. d'8 d'2 |
 a'1. a'2 a'1. a'2 a'1 |
 r2 a'1 a'2 a'1. a'2 a'1 |

    }
  >>
 
\new Staff
  <<
    \new Voice {
      \clef "treble_8"
      \time 4/2
      g2 g4 f4 g4. g8 f2 |
 f2 g4 a4 b2 a2 |
 f'2 b1 a2 a2. a4 a1 |
 a2 g2 e'1 f'1 |
 a2 a1 a2 a1 a1 |
 a2 a4 a4 a2 a2 |
 b1 a1 f'1 |
 f'2 g'2 e'2. e'4 f'1 |
 e'1. f'2 g'1. g'2 a'1 |
 b'4. a'8 |
 a'1 g'2 a'2 g'2. g'4 a'1 |
 a'1 a'1 a'1 a'1 |
 g'4 b8 f'8 g'4. g'8 f'1 |
 r2 e'1 f'2 e'1. e'2 f'1 |
 e'1. e'2 f'1 |
 f'4 g'8 f'8 e'4. e'8 f'1 |
 a1 g2 a2 g2. g4 a1 |
 a1 a2 a2 a1. a2 a1 |
 a1 a2 a2 a1 a1 |
 r4 e'4 d'4. e'8 f'1 |
 r4 f'4 g'4 f'4 e'4. e'8 f'2 |
 a4. a2 b1 |
 g2 b4 f'4 g'4. g'8 f'2 |
 e'1. f'2 e'1. e'2 f'1 |
 r2 e'1 f'2 e'1. e'2 f'1 |

    }
  >>
 
\new Staff
  <<
    \new Voice {
      \clef "bass"
      \time 4/2
      g2 g4 d4 g4. g8 d2 |
 d2 g4 f4 g2 d2 |
 d2 g1 d2 a2. a4 d1 |
 f2 g2 a1 d1 |
 a2 f1 d2 a1 d1 |
 a2 f4 d4 a2 d2 |
 g1 a1 b1 |
 b2 g2 a2. a4 d1 |
 a1. f2 e1. e2 a1 |
 g4. d8 |
 a1 e2 a2 e2. e4 a1 |
 a1 d1 a1 d1 |
 g4 g8 d8 g4. g8 d1 |
 r2 a1 d2 a1. a2 d1 |
 a1. a2 d1 |
 d4 g8 d8 a4. a8 d1 |
 a1 e2 a2 e2. e4 a1 |
 a1 f2 d2 a1. a2 d1 |
 a1 f2 d2 a1 d1 |
 r4 e4 b4. a8 d1 |
 r4 d4 g4 d4 a4. a8 d2 |
 d4. d2 g1 |
 g2 g4 d4 g4. g8 d2 |
 a1. d2 a1. a2 d1 |
 r2 a1 d2 a1. a2 d1 |

    }
  >>

  >>
>>
}
 
\score {
<<
  \new ChoirStaff
  <<
    
\new Staff
  <<
    \new Voice {
      \clef "treble"
      \time 4/2
      a'1 b'2 a'2 a'2. a'4 a'1 |
 f''2 e''1 c''2 d''1 |
 b'1. a'2 b'2 a'2 g'1 f'1 |
 f'2. e'4 c''2. c''4 d''1 |
 a'4 g'8 f'8 e'4. e'8 f'1 |
 r2 b'2 a'1. a'2 |
 f''2. e''4 |

    }
  >>
 
\new Staff
  <<
    \new Voice {
      \clef "treble"
      \time 4/2
      d'1 d'2 d'2 c'2. c'4 d'1 |
 d'2 b1 a2 a1 |
 d'1. d'2 d'2 d'2 d'1 d'1 |
 c'2. b4 a2. a4 a1 |
 d'4 d'8 d'8 c'4. c'8 d'1 |
 r2 d'2 c'1. c'2 |
 d'2. c'4 |

    }
  >>
 
\new Staff
  <<
    \new Voice {
      \clef "treble_8"
      \time 4/2
      f'1 g'2 f'2 e'2. e'4 f'1 |
 a2 g1 e'2 f'1 |
 g'1. f'2 g'2 a'2 b'1 a'1 |
 a'2. g'4 e'2. e'4 f'1 |
 f'4 b8 a8 a4. a8 a1 |
 r2 g2 e'1. e'2 |
 a2. a4 |

    }
  >>
 
\new Staff
  <<
    \new Voice {
      \clef "bass"
      \time 4/2
      d1 g2 d2 a2. a4 d1 |
 d2 g1 a2 d1 |
 g1. d2 g2 f2 g1 d1 |
 f2. g4 a2. a4 d1 |
 d4 g8 d8 a4. a8 d1 |
 r2 g2 a1. d2 |
 d2. a4 |

    }
  >>

  >>
>>
}

