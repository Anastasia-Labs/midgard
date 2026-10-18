import { execFileSync } from "node:child_process";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

/**
 * Typed front end for `cml-max-depth-child-v1.mjs`.
 *
 * Every maximum-depth operation runs in its own short-lived child, on the
 * machine stack `stackSizeKb` sets (`--stack-size`, which cannot be set from
 * inside a running worker). No vitest pool option, no `NODE_OPTIONS`, no
 * change to how any other suite executes.
 */

const HERE = dirname(fileURLToPath(import.meta.url));

/** The child's default machine stack. Kept from `@lucid-evolution/cml`
 * 0.1.0, which needed more than 1,400 KB at depth 4,043; 0.1.2 does not. */
export const MAX_DEPTH_V8_STACK_SIZE_KB = 2_000;

export const CML_MAX_DEPTH_CHILD_SCRIPT = join(
  HERE,
  "cml-max-depth-child-v1.mjs",
);

export type MaxDepthCmlRequest =
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

export type MaxDepthCmlResult = {
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

export const runMaxDepthCmlOperation = (
  request: MaxDepthCmlRequest,
  {
    stackSizeKb = MAX_DEPTH_V8_STACK_SIZE_KB,
    timeoutMs = 300_000,
  }: { readonly stackSizeKb?: number | null; readonly timeoutMs?: number } = {},
): MaxDepthCmlResult => {
  const scratch = mkdtempSync(join(tmpdir(), "midgard-c26-maxdepth-"));
  const requestPath = join(scratch, "request.json");
  writeFileSync(requestPath, JSON.stringify(encodeBigints(request)), "utf8");
  const nodeArgs = [
    ...(stackSizeKb === null ? [] : [`--stack-size=${stackSizeKb}`]),
    CML_MAX_DEPTH_CHILD_SCRIPT,
    requestPath,
  ];
  const stdout = execFileSync(process.execPath, nodeArgs, {
    encoding: "utf8",
    timeout: timeoutMs,
    maxBuffer: 64 * 1024 * 1024,
  });
  return JSON.parse(stdout) as MaxDepthCmlResult;
};
