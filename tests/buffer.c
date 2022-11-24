#include "config.h"

#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <fcntl.h>

#include <espeak-ng/espeak_ng.h>

/* https://github.com/espeak-ng/espeak-ng/issues/1554 */

/* You can read this by first converting to wav with
 * sox -e signed-integer -b 16 --endian little -c 1 -r 22050 audio.raw audio.wav
 */
#define OUTPUT "audio.raw"

static int mycallback(short *wav, int numsamples, espeak_EVENT *events)
{
	int f = open(OUTPUT, O_WRONLY|O_APPEND|O_CREAT, 0666);
	write(f, wav, numsamples*2);
	close(f);
	printf("wrote %d samples\n", numsamples);
	(void) events;
	return 0;
}

#define CHECK(f) \
	if (ret != EE_OK) { fprintf(stderr,"failed %s: %d\n", f, ret); exit(1); }

int main(void) {
	unlink(OUTPUT);

	espeak_ERROR ret;
	espeak_Initialize(AUDIO_OUTPUT_SYNCHRONOUS, 3000, NULL, 0);
	espeak_SetSynthCallback(mycallback);
	ret = espeak_SetParameter(espeakRATE, 449, 0);
	CHECK("rate");
	ret = espeak_SetVoiceByName("ru");
	CHECK("voice");
	espeak_ng_SetRandSeed(1);

	const char *s = "<speak><mark name=0:4/>read <mark name=5:10/>bytes <mark name=11:15/>from <mark name=16:21/>their <mark name=22:27/>input <mark name=28:31/>and <mark name=32:37/>write <mark name=38:41/>the <mark name=42:47/>bytes</speak>";

	espeak_Synth(s, strlen(s)+1, 0, POS_CHARACTER, 0, espeakSSML | espeakCHARS_UTF8, NULL, NULL);
	espeak_Synchronize();
	espeak_Terminate();
}
