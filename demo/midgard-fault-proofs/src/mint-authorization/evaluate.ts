import {
  decodeMidgardNativeScript,
  hashMidgardNativeScriptScanFrame,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH,
  MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES,
  type MidgardNativeScriptScanFrame,
  readMidgardNativeScriptStructureToken,
  verifyMidgardNativeScript,
} from "@al-ft/midgard-core";
import type {
  MintAuthorizationEvaluateOperation,
  MintAuthorizationEvaluateState,
  MintAuthorizationFrame,
} from "@al-ft/midgard-sdk";

const frameWire = (
  frame: MidgardNativeScriptScanFrame,
): MintAuthorizationFrame => ({
  tail: frame.tail.toString("hex"),
  kind: BigInt(frame.kind),
  child_count: BigInt(frame.childCount),
  remaining: BigInt(frame.remaining),
  valid_count: BigInt(frame.validCount),
  required: frame.required,
});

/** Reconstructs only canonical batches from retained policy bytes; no journal supplies evaluator state. */
export function* mintAuthorizationEvaluationBatches(
  initial: MintAuthorizationEvaluateState,
  bytes: Buffer,
) {
  let state = initial;
  const frames: MidgardNativeScriptScanFrame[] = [];
  const signerHashes = new Set(initial.signer_hashes);
  while (
    state.cursor !== state.script_length ||
    state.stack_depth !== 0n ||
    state.result === -1n
  ) {
    const before = state;
    const operations: MintAuthorizationEvaluateOperation[] = [];
    for (let count = 0; count < 16; count++) {
      if (state.result === -1n) {
        const cursor = Number(state.cursor);
        const token = readMidgardNativeScriptStructureToken({
          control: {
            version: 1,
            stage: 0,
            startOffset: 0,
            cursor,
            endOffset: bytes.length,
            stackRoot: Buffer.from(state.stack_root, "hex"),
            stackDepth: Number(state.stack_depth),
            nodeCount: Number(state.node_count),
          },
          window: bytes,
          windowOffset: cursor,
        });
        if (
          token.nextOffset > bytes.length ||
          state.node_count >= BigInt(MIDGARD_NATIVE_SCRIPT_SCAN_MAX_NODES)
        )
          throw new Error(
            "mint authorization native token exceeds canonical bounds",
          );
        operations.push("Token");
        state = {
          ...state,
          cursor: BigInt(token.nextOffset),
          node_count: state.node_count + 1n,
        };
        if (
          (token.kind === 1 || token.kind === 2 || token.kind === 3) &&
          token.childCount > 0
        ) {
          if (frames.length >= MIDGARD_NATIVE_SCRIPT_SCAN_MAX_DEPTH)
            throw new Error(
              "mint authorization native depth exceeds canonical bound",
            );
          const frame: MidgardNativeScriptScanFrame = {
            tail: Buffer.from(state.stack_root, "hex"),
            kind: token.kind,
            childCount: token.childCount,
            remaining: token.childCount,
            validCount: 0,
            required: token.required,
          };
          frames.push(frame);
          state = {
            ...state,
            stack_root: hashMidgardNativeScriptScanFrame(frame).toString("hex"),
            stack_depth: state.stack_depth + 1n,
          };
        } else {
          const leaf = decodeMidgardNativeScript(
            bytes.subarray(cursor, token.nextOffset),
          );
          const valid = verifyMidgardNativeScript(leaf.script, {
            witnessSigners: signerHashes,
            validityIntervalStart:
              initial.validity_interval_start < 0n
                ? undefined
                : initial.validity_interval_start,
            validityIntervalEnd:
              initial.validity_interval_end < 0n
                ? undefined
                : initial.validity_interval_end,
          });
          state = { ...state, result: valid ? 1n : 0n };
        }
      } else if (state.stack_depth > 0n) {
        const frame = frames.pop();
        if (
          frame === undefined ||
          hashMidgardNativeScriptScanFrame(frame).toString("hex") !==
            state.stack_root
        )
          throw new Error("mint authorization native frame commitment changed");
        operations.push({ Frame: { frame: frameWire(frame) } });
        const validCount = frame.validCount + Number(state.result);
        const remaining = frame.remaining - 1;
        if (remaining > 0) {
          const next = { ...frame, validCount, remaining };
          frames.push(next);
          state = {
            ...state,
            stack_root: hashMidgardNativeScriptScanFrame(next).toString("hex"),
            result: -1n,
          };
        } else {
          const valid =
            frame.kind === 1
              ? validCount === frame.childCount
              : frame.kind === 2
                ? validCount > 0
                : BigInt(validCount) >= frame.required;
          state = {
            ...state,
            stack_root: frame.tail.toString("hex"),
            stack_depth: state.stack_depth - 1n,
            result: valid ? 1n : 0n,
          };
        }
      } else break;
    }
    if (operations.length === 0)
      throw new Error("mint authorization native script has trailing bytes");
    yield { before, after: state, operations };
  }
  if (state.result !== 0n)
    throw new Error("mint authorization native policy is satisfied");
}
