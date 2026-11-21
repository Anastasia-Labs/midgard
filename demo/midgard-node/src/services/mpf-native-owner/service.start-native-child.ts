import { NATIVE_MPF_OWNER_DEFAULT_CAPS, NativeMpfRpcKind } from "./protocol.js";
import {
  assertPinnedOwnerBinary,
  NativeChildRpc,
} from "./service.native-child-rpc.js";
import {
  digest,
  FULL_INDEX_HEADER_BYTES,
  HASH_BYTES,
  LOAD_DIGEST_DOMAIN,
  type NormalizedNativeMpfOwnerServiceOptions,
} from "./service.normalize-owner-options.js";

export const startNativeChild = async ({
  options,
  fullIndex,
  marker,
}: {
  readonly options: NormalizedNativeMpfOwnerServiceOptions;
  readonly fullIndex: Buffer;
  readonly marker: string;
}): Promise<NativeChildRpc> => {
  // Restarts and restores spawn the path again; the file there may have been
  // replaced since create() checked it, so every spawn re-verifies the pin.
  await assertPinnedOwnerBinary(options.binaryPath, options.binarySha256);
  const rpc = new NativeChildRpc(
    options.binaryPath,
    options.binarySha256,
    options.maxFrameBytes,
    options.requestTimeoutMs,
    options.onChildSpawnForTests,
  );
  try {
    await rpc.handshake();
    const header = fullIndex.subarray(0, FULL_INDEX_HEADER_BYTES);
    await rpc.request(
      NativeMpfRpcKind.LoadBegin,
      header,
      new Set([NativeMpfRpcKind.LoadBegin]),
    );
    for (
      let offset = FULL_INDEX_HEADER_BYTES;
      offset < fullIndex.length;
      offset += options.maxChunkBytes
    ) {
      await rpc.request(
        NativeMpfRpcKind.LoadChunk,
        fullIndex.subarray(
          offset,
          Math.min(fullIndex.length, offset + options.maxChunkBytes),
        ),
        new Set([NativeMpfRpcKind.LoadChunk]),
      );
    }
    const ready = await rpc.request(
      NativeMpfRpcKind.LoadEnd,
      digest(LOAD_DIGEST_DOMAIN, fullIndex),
      new Set([NativeMpfRpcKind.Ready]),
    );
    const readyPayload = Buffer.from(ready.payload);
    if (
      readyPayload.length !== 72 ||
      readyPayload.subarray(0, HASH_BYTES).toString("hex") !== marker
    ) {
      throw new Error("Native MPF Ready marker/diagnostics are invalid");
    }
    const readyValue = (offset: number): number => {
      const value = readyPayload.readBigUInt64LE(offset);
      if (value > BigInt(Number.MAX_SAFE_INTEGER)) {
        throw new Error("Native MPF Ready diagnostic exceeds safe integer");
      }
      return Number(value);
    };
    const readyNodes = readyValue(32);
    const readyResidentBytes = readyValue(48);
    const readyRssBytes = readyValue(56) * 1024;
    const readyPeakRssBytes = readyValue(64) * 1024;
    if (
      readyNodes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentNodes ||
      readyResidentBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes ||
      readyRssBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes ||
      readyPeakRssBytes > NATIVE_MPF_OWNER_DEFAULT_CAPS.maxResidentBytes
    ) {
      throw new Error(
        `Native MPF Ready cap exceeded: nodes=${readyNodes.toString()},resident_bytes=${readyResidentBytes.toString()},rss_bytes=${readyRssBytes.toString()},peak_rss_bytes=${readyPeakRssBytes.toString()}`,
      );
    }
    return rpc;
  } catch (error) {
    await rpc.close().catch(() => undefined);
    throw error;
  }
};
