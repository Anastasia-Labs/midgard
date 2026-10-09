/** An L1 source answer that says nothing about the chain: the socket or
 * endpoint was unreachable, slow or closed, or an indexer has not caught up
 * with a block another source already served. Retrying after fresh source
 * authentication and intersection cannot admit different history, so a
 * harness reader may retry on it. Anything that DOES say
 * something about the chain (a genesis mismatch, a missing intersection, a
 * broken ancestry link, a malformed answer) is never one of these. */
export class L1SourceUnavailable extends Error {}

/** One Ogmios request outlived its deadline on a socket that is still open. */
export class OgmiosRequestTimeout extends L1SourceUnavailable {}

/** Kupo has no match or checkpoint for a point Ogmios already served: the
 * index is behind the node, which is not evidence that the point is absent. */
export class KupoNotYetIndexed extends L1SourceUnavailable {}
