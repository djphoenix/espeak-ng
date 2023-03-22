#pragma once

#include "synthesize.hpp"
#include "voice.hpp"
#include <speechPlayer.h>

namespace espeak {

void KlattInitSP(void);
void KlattResetSP(void);
void KlattFiniSP(void);
int Wavegen_KlattSP(WGEN_DATA *wdata, voice_t *wvoice, int length, int resume, frame_t *fr1, frame_t *fr2);

}