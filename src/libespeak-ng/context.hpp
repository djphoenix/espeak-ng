/*
 * Copyright (C) 2005 to 2007 by Jonathan Duddington
 * email: jonsd@users.sourceforge.net
 * Copyright (C) 2013-2015 Reece H. Dunn
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
 * along with this program; if not, see: <http://www.gnu.org/licenses/>.
 */

#pragma once

#include <espeak-ng/espeak_ng_ctx.h>

namespace espeak {
    struct context_t {
        espeak_ng_ERROR_CONTEXT error_context;


        static context_t& global();

        espeak_ng_ERROR_CONTEXT GetError();
        const espeak_VOICE **ListVoices(espeak_VOICE *voice_spec);
        void InitializePath(const char *path);
        espeak_ng_STATUS Initialize();
        espeak_ng_STATUS InitializeOutput(
            espeak_ng_OUTPUT_MODE output_mode,
            int buffer_length,
            const char *device
        );
        int GetSampleRate() const;

        espeak_ng_STATUS SetRandSeed(long seed);

        espeak_ng_STATUS SetParameter(espeak_PARAMETER parameter, int value, int relative);
        espeak_ng_STATUS SetPhonemeEvents(int enable, int ipa);
        espeak_ng_STATUS SetPunctuationList(const wchar_t *punctlist);
        espeak_ng_STATUS SetVoiceByName(const char *name);
        espeak_ng_STATUS SetVoiceByFile(const char *filename);

        espeak_ng_STATUS SetVoiceByProperties(espeak_VOICE *voice_selector);
        espeak_ng_STATUS SpeakKeyName(const char *key_name);
        espeak_ng_STATUS SpeakCharacter(wchar_t character);
        espeak_ng_STATUS Cancel();
        espeak_ng_STATUS Synchronize();
        espeak_ng_STATUS Terminate();
        espeak_ng_STATUS SetOutputHooks(espeak_ng_OUTPUT_HOOKS* hooks);
        espeak_ng_STATUS SetConstF0(int f0);
        const char *     TextToPhonemes(const void **textptr, int textmode, int phonememode);

        espeak_ng_STATUS SetSynthCallback(t_espeak_callback* SynthCallback);
        espeak_ng_STATUS SetUriCallback(int (*UriCallback)(int, const char*, const char*));
        espeak_ng_STATUS SetPhonemeCallback(int (*PhonemeCallback)(const char *));

        espeak_ng_STATUS Synthesize(
            const void *text,
            size_t size,
            unsigned int position,
            espeak_POSITION_TYPE position_type,
            unsigned int end_position,
            unsigned int flags,
            unsigned int *unique_identifier,
            void *user_data
        );

        espeak_ng_STATUS SynthesizeMark(
            const void *text,
            size_t size,
            const char *index_mark,
            unsigned int end_position,
            unsigned int flags,
            unsigned int *unique_identifier,
            void *user_data
        );

        espeak_ng_STATUS CompileDictionary(
            const char *dsource,
            const char *dict_name,
            FILE *log,
            int flags
        );

        espeak_ng_STATUS CompileMbrolaVoice(
            const char *path,
            FILE *log
        );

        espeak_ng_STATUS CompilePhonemeDataPath(
            long rate,
            const char *source_path,
            const char *destination_path,
            FILE *log
        );

        espeak_ng_STATUS CompileIntonationPath(
            const char *source_path,
            const char *destination_path,
            FILE *log
        );
    };
}

struct espeak_ng_CONTEXT : public espeak::context_t {};
