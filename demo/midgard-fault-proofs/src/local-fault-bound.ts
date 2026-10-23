/**
 * How many attempts in a row may end on a local fault (the process's own
 * computation failed on a block, which says nothing about the block) with no
 * progress past it before the process stops and exits non-zero, its
 * supervisor's restart being the retry after that.
 */
export const LOCAL_FAULT_RUN_LIMIT = 8;
