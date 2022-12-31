
#include <stdlib.h>
#include <string.h>
#include "../include/libstemmer.h"
#include "../runtime/api.h"
#include "../src_c/stem_ISO_8859_1_english.h"

extern struct SN_env *
english_ISO_8859_1_stemmer_new()
{
    struct SN_env * sn_env = english_ISO_8859_1_create_env();
    if (sn_env == NULL)
    {
        english_ISO_8859_1_stemmer_delete(sn_env);
        return NULL;
    }

    return sn_env;
}

void
english_ISO_8859_1_stemmer_delete(struct SN_env * sn_env)
{
    if (sn_env == 0) return;
    english_ISO_8859_1_close_env(sn_env);
}

const sb_symbol *
english_ISO_8859_1_stemmer_stem(struct SN_env * sn_env, const sb_symbol * word, int size)
{
    int ret;
    if (SN_set_current(sn_env, size, (const symbol *)(word)))
    {
        sn_env->l = 0;
        return NULL;
    }
    ret = english_ISO_8859_1_stem(sn_env);
    if (ret < 0) return NULL;
    sn_env->p[sn_env->l] = 0;
    return (const sb_symbol *)(sn_env->p);
}

int
english_ISO_8859_1_stemmer_length(struct SN_env * sn_env)
{
    return sn_env->l;
}
