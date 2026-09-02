import { spawn } from "node:child_process";

import { CML } from "@lucid-evolution/lucid";

import type { makeFaultProofEmulatorHarnessV1 } from "./support/emulator/harness.js";

const worker = new URL(
  "./support/isolated-uplc-evaluator-v1.cjs",
  import.meta.url,
);
const tags = [
  "spend",
  "mint",
  "publish",
  "withdraw",
  "vote",
  "propose",
] as const;

const run = async (input: string): Promise<string> =>
  await new Promise((resolve, reject) => {
    const child = spawn(process.execPath, [worker.pathname], {
      stdio: ["pipe", "pipe", "pipe"],
    });
    const stdout: Buffer[] = [];
    const stderr: Buffer[] = [];
    child.stdout.on("data", (chunk: Buffer) => stdout.push(chunk));
    child.stderr.on("data", (chunk: Buffer) => stderr.push(chunk));
    child.on("error", reject);
    child.on("close", (code) => {
      const error = Buffer.concat(stderr).toString("utf8");
      if (code !== 0)
        return reject(
          new Error(
            `isolated protected-output evaluator exited ${String(code)}: ${error}`,
          ),
        );
      const output = Buffer.concat(stdout).toString("utf8");
      return output.length === 0
        ? reject(
            new Error(
              `isolated protected-output evaluator was empty: ${error}`,
            ),
          )
        : resolve(output);
    });
    child.stdin.end(input);
  });

/** One fresh wasm32 evaluator process per transaction; avoids UPLC arena leaks. */
export const makeProtectedOutputSignerIsolatedEvaluatorV1 = () => ({
  name: "protected-output-signer-isolated-uplc-v1",
  evaluate: async ({
    tx,
    additionalUTxOs,
    context,
  }: Parameters<
    NonNullable<
      NonNullable<
        NonNullable<
          Parameters<typeof makeFaultProofEmulatorHarnessV1>[0]
        >["lucidOptions"]
      >["evaluator"]
    >["evaluate"]
  >[0]) => {
    const output = await run(
      JSON.stringify(
        {
          tx,
          additionalUTxOs,
          costModels: context.costModels.to_cbor_hex(),
          maxTxExSteps: context.protocolParameters.maxTxExSteps.toString(),
          maxTxExMem: context.protocolParameters.maxTxExMem.toString(),
          zeroTime: context.slotConfig.zeroTime.toString(),
          zeroSlot: context.slotConfig.zeroSlot.toString(),
          slotLength: context.slotConfig.slotLength,
        },
        (_key, value) =>
          typeof value === "bigint" ? { $bigint: value.toString() } : value,
      ),
    );
    return (JSON.parse(output) as string[]).map((hex) => {
      const redeemer = CML.LegacyRedeemer.from_cbor_hex(hex);
      const tag = tags[redeemer.tag()];
      if (tag === undefined)
        throw new Error(`unknown isolated redeemer tag ${redeemer.tag()}`);
      return {
        ex_units: {
          mem: Number(redeemer.ex_units().mem()),
          steps: Number(redeemer.ex_units().steps()),
        },
        redeemer_index: Number(redeemer.index()),
        redeemer_tag: tag,
      };
    });
  },
});
