
/* Make header file work when included from C++ */
#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned char sb_symbol;

struct SN_env * english_ISO_8859_1_stemmer_new();

void english_ISO_8859_1_stemmer_delete(struct SN_env * sn_env);

const sb_symbol * english_ISO_8859_1_stemmer_stem(struct SN_env * sn_env, const sb_symbol * word, int size);

int english_ISO_8859_1_stemmer_length(struct SN_env * sn_env);

#ifdef __cplusplus
}
#endif

