import { NativeMpfRpcKind } from "./protocol.js";
import {
  assertPinnedOwnerBinary,
  NativeChildRpc,
} from "./service.native-child-rpc.js";
import type { NormalizedNativeMpfOwnerServiceOptions } from "./service.normalize-owner-options.js";
import { decodeReady, encodeSetRoot } from "./service.rpc-payloads.js";

/** Sets `rpc`'s root to `root`; the child refuses while a generation is
 * active. Its cache is kept: entries are keyed by content hash. */
export const setChildRoot = async (
  rpc: NativeChildRpc,
  root: string,
): Promise<void> => {
  const ready = await rpc.request(
    NativeMpfRpcKind.SetRoot,
    encodeSetRoot(root),
    new Set([NativeMpfRpcKind.Ready]),
  );
  decodeReady(ready.payload, root);
};

/** A new child with an empty cache and `root` as its root. It reads no
 * trie node: the first apply asks for the nodes it needs. */
export const startNativeChild = async ({
  options,
  root,
}: {
  readonly options: NormalizedNativeMpfOwnerServiceOptions;
  readonly root: string;
}): Promise<NativeChildRpc> => {
  // Restarts spawn the path again; the file there may have been replaced
  // since create() checked it, so every spawn re-verifies the pin.
  await assertPinnedOwnerBinary(options.binaryPath, options.binarySha256);
  const rpc = new NativeChildRpc(
    options.binaryPath,
    options.binarySha256,
    options.maxFrameBytes,
    options.requestTimeoutMs,
    options.onChildSpawnForTests,
  );
  try {
    await rpc.handshake(options.nodeCacheBytes);
    await setChildRoot(rpc, root);
    return rpc;
  } catch (error) {
    await rpc.close().catch(() => undefined);
    throw error;
  }
};
