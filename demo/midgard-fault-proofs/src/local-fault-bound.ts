/**
 * How many attempts in a row may end on a local fault (the process's own
 * computation failed on a block, which says nothing about the block) with no
 * progress past it before the process stops and exits non-zero, its
 * supervisor's restart being the retry after that. A held local fault (the
 * event replay's own throw in the node, any replay local fault in the
 * watcher) never ends the process: after this many failures in a row on its
 * block, whatever their message, its hold is named stuck, and it is still
 * retried.
 */
export const LOCAL_FAULT_RUN_LIMIT = 8;
