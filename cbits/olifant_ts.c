/* A thin shim over the tree-sitter C runtime (cbits/tree-sitter, compiled
 * in), so Haskell's FFI, which cannot pass structs by value, can hold a
 * TSNode by pointer. The grammar is passed in (a TSLanguage from the
 * grammar table in Olifant.Language.TreeSitter). */
#include <stdlib.h>
#include <string.h>
#include <tree_sitter/api.h>

size_t ots_node_size(void) { return sizeof(TSNode); }

/* A parse of a UTF-8 text; NULL when the language cannot be set (ABI). */
TSTree *ots_parse(const TSLanguage *lang, const char *src, uint32_t len) {
  TSParser *p = ts_parser_new();
  if (!ts_parser_set_language(p, lang)) { ts_parser_delete(p); return NULL; }
  TSTree *t = ts_parser_parse_string(p, NULL, src, len);
  ts_parser_delete(p);
  return t;
}

void ots_root(TSTree *t, TSNode *out) { *out = ts_tree_root_node(t); }

uint32_t ots_start_byte(const TSNode *n) { return ts_node_start_byte(*n); }
uint32_t ots_end_byte(const TSNode *n) { return ts_node_end_byte(*n); }
/* row and column (bytes) of a node's start and end, into out[4] */
void ots_points(const TSNode *n, uint32_t *out) {
  TSPoint s = ts_node_start_point(*n), e = ts_node_end_point(*n);
  out[0] = s.row; out[1] = s.column; out[2] = e.row; out[3] = e.column;
}
int ots_is_null(const TSNode *n) { return ts_node_is_null(*n); }
int ots_is_named(const TSNode *n) { return ts_node_is_named(*n); }
int ots_is_extra(const TSNode *n) { return ts_node_is_extra(*n); }
const char *ots_type(const TSNode *n) { return ts_node_type(*n); }
int ots_eq(const TSNode *a, const TSNode *b) { return ts_node_eq(*a, *b); }
void ots_parent(const TSNode *n, TSNode *out) { *out = ts_node_parent(*n); }
void ots_field(const TSNode *n, const char *name, uint32_t len, TSNode *out) {
  *out = ts_node_child_by_field_name(*n, name, len);
}
uint32_t ots_child_count(const TSNode *n) { return ts_node_child_count(*n); }
void ots_child(const TSNode *n, uint32_t i, TSNode *out) { *out = ts_node_child(*n, i); }

/* A query over one language; NULL and *err_offset, *err_type on an error. */
TSQuery *ots_query(const TSLanguage *lang, const char *src, uint32_t len, uint32_t *err_offset, uint32_t *err_type) {
  TSQueryError e = TSQueryErrorNone;
  TSQuery *q = ts_query_new(lang, src, len, err_offset, &e);
  *err_type = (uint32_t)e;
  return q;
}

const char *ots_capture_name(const TSQuery *q, uint32_t i, uint32_t *len) {
  return ts_query_capture_name_for_id(q, i, len);
}

TSQueryCursor *ots_exec(const TSQuery *q, const TSNode *n) {
  TSQueryCursor *c = ts_query_cursor_new();
  ts_query_cursor_exec(c, q, *n);
  return c;
}

/* The next match: its pattern and captures, at most @max of them (nodes into
 * @nodes, capture ids into @ids). Returns the capture count, or -1 at the end. */
int ots_next(TSQueryCursor *c, uint32_t max, TSNode *nodes, uint32_t *ids, uint32_t *pattern) {
  TSQueryMatch m;
  if (!ts_query_cursor_next_match(c, &m)) return -1;
  *pattern = m.pattern_index;
  uint32_t n = m.capture_count < max ? m.capture_count : max;
  for (uint32_t i = 0; i < n; i++) { nodes[i] = m.captures[i].node; ids[i] = m.captures[i].index; }
  return (int)n;
}

uint32_t ots_capture_count(const TSQuery *q) { return ts_query_capture_count(q); }

/* Keep only the captures flagged in @keep (by capture id), and the patterns
 * that still have one; the rest cost nothing at match time. */
void ots_prune(TSQuery *q, const uint8_t *keep) {
  uint32_t nc = ts_query_capture_count(q), np = ts_query_pattern_count(q);
  for (uint32_t p = 0; p < np; p++) {
    int any = 0;
    for (uint32_t c = 0; c < nc && !any; c++)
      if (keep[c] && ts_query_capture_quantifier_for_id(q, p, c) != TSQuantifierZero) any = 1;
    if (!any) ts_query_disable_pattern(q, p);
  }
  for (uint32_t c = 0; c < nc; c++)
    if (!keep[c]) { uint32_t len; const char *s = ts_query_capture_name_for_id(q, c, &len); ts_query_disable_capture(q, s, len); }
}
