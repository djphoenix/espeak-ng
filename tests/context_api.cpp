/*
 * Copyright (C) 2017 Reece H. Dunn
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write see:
 *             <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

#include <espeak-ng/espeak_ng_ctx.h>
#include <espeak-ng/speak_lib.h>
#include <espeak-ng/encoding.h>

static void test_run() {
	espeak_ng_CONTEXT *ctx;
	const char *test = "One two three.";

	ctx = espeak_ng_ctx_New();
	assert(ctx != NULL);

	espeak_ng_ctx_InitializePath(ctx, NULL);

	assert(espeak_ng_ctx_Initialize(ctx) == ENS_OK);

	assert(espeak_ng_ctx_InitializeOutput(ctx, ENOUTPUT_MODE_SYNCHRONOUS, 0, NULL) == ENS_OK);

	assert(espeak_ng_ctx_GetSampleRate(ctx) == 22050);

	assert(espeak_ng_ctx_SetParameter(ctx, espeakRATE, 120, 0) == ENS_OK);

	assert(espeak_ng_ctx_SetPhonemeEvents(ctx, 1, 0) == ENS_OK);

	assert(espeak_ng_ctx_SetPunctuationList(ctx, L"") == ENS_OK);

	char vn[] = "en";

	espeak_VOICE sel;
	memset(&sel, 0, sizeof(sel));
	sel.name = vn;
	assert(espeak_ng_ctx_SetVoiceByProperties(ctx, &sel) == ENS_OK);

	assert(espeak_ng_ctx_SetVoiceByName(ctx, "en+m1") == ENS_OK);

	assert(espeak_ng_ctx_Synthesize(ctx, test, strlen(test)+1, 0, POS_CHARACTER, 0, espeakCHARS_AUTO, NULL, NULL) == ENS_OK);
	assert(espeak_ng_ctx_Synchronize(ctx) == ENS_OK);

	assert(espeak_ng_ctx_SpeakKeyName(ctx, "A") == ENS_OK);
	assert(espeak_ng_ctx_Synchronize(ctx) == ENS_OK);

	assert(espeak_ng_ctx_SpeakCharacter(ctx, L'A') == ENS_OK);
	assert(espeak_ng_ctx_Synchronize(ctx) == ENS_OK);

	assert(espeak_ng_ctx_Terminate(ctx) == ENS_OK);

	espeak_ng_ctx_Free(ctx);
}

static void* test_loop(void*) {
	for (int i = 0; i < 10; i++) test_run();
	return NULL;
}

int main(int argc, char **argv) {
	unsigned long num_threads = 8;
	if (argc > 1) {
		num_threads = strtoul(argv[1], NULL, 10);
	}

	pthread_t threads[256];

	for (unsigned long i = 0; i < num_threads; i++) {
		pthread_create(&threads[i], NULL, test_loop, NULL);
	}
	for (unsigned long i = 0; i < num_threads; i++) {
		pthread_join(threads[i], NULL);
	}

	return EXIT_SUCCESS;
}
