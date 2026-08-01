import { execFileSync } from "node:child_process";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

/**
 * Typed front end for `cml-max-depth-child-v1.mjs`.
 *
 * Maximum-depth Plutus Data work needs BOTH raised budgets:
 *
 *   - the CML wasm shadow stack, raised at install time by
 *     `demo/scripts/patch-cml-wasm-stack.mjs` (hash-pinned), and
 *   - V8's machine stack, raised with `--stack-size` (measured floor 1,400 KB
 *     at depth 4,043; 2,000 KB is used for headroom).
 *
 * `--stack-size` cannot be set from inside a running worker, and a CML wasm
 * trap poisons the `WebAssembly.Instance` for the remainder of the process, so
 * every max-depth operation runs in its own short-lived child. This is what
 * keeps the flag scoped to the suites that need it: no vitest pool option, no
 * `NODE_OPTIONS`, no change to how any other suite executes.
 */

const HERE = dirname(fileURLToPath(import.meta.url));

export const MAX_DEPTH_V8_STACK_SIZE_KB_V1 = 2_000;

export const CML_MAX_DEPTH_CHILD_SCRIPT_V1 = join(
  HERE,
  "cml-max-depth-child-v1.mjs",
);

export type MaxDepthCmlRequestV1 =
  | {
      readonly operation: "plutusDataParse";
      readonly cmlMainPath: string;
      readonly depth: number;
    }
  | {
      readonly operation: "transactionParse";
      readonly cmlMainPath: string;
      readonly signedTxHex: string;
    }
  | {
      readonly operation: "emulatorAdmission";
      readonly signedTxHex: string;
      readonly expectedDatumHex: string;
      readonly account: {
        readonly privateKey: string;
        readonly address: string;
        readonly assets: Record<string, unknown>;
      };
      readonly protocolParameters: Record<string, unknown>;
    };

export type MaxDepthCmlResultV1 = {
  readonly ok: boolean;
  readonly errorName?: string;
  readonly message?: string;
} & Record<string, unknown>;

/** JSON is bigint-blind; the child revives `{ "__bigint": "…" }` back to BigInt. */
const encodeBigints = (value: unknown): unknown => {
  if (typeof value === "bigint") return { __bigint: value.toString() };
  if (Array.isArray(value)) return value.map(encodeBigints);
  if (value && typeof value === "object") {
    return Object.fromEntries(
      Object.entries(value as Record<string, unknown>).map(([key, entry]) => [
        key,
        encodeBigints(entry),
      ]),
    );
  }
  return value;
};

export const runMaxDepthCmlOperationV1 = (
  request: MaxDepthCmlRequestV1,
  {
    stackSizeKb = MAX_DEPTH_V8_STACK_SIZE_KB_V1,
    timeoutMs = 300_000,
  }: { readonly stackSizeKb?: number | null; readonly timeoutMs?: number } = {},
): MaxDepthCmlResultV1 => {
  const scratch = mkdtempSync(join(tmpdir(), "midgard-c26-maxdepth-"));
  const requestPath = join(scratch, "request.json");
  writeFileSync(requestPath, JSON.stringify(encodeBigints(request)), "utf8");
  const nodeArgs = [
    ...(stackSizeKb === null ? [] : [`--stack-size=${stackSizeKb}`]),
    CML_MAX_DEPTH_CHILD_SCRIPT_V1,
    requestPath,
  ];
  const stdout = execFileSync(process.execPath, nodeArgs, {
    encoding: "utf8",
    timeout: timeoutMs,
    maxBuffer: 64 * 1024 * 1024,
  });
  return JSON.parse(stdout) as MaxDepthCmlResultV1;
};
