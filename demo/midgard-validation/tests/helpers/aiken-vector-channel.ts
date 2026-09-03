import { mkdirSync, writeFileSync } from "node:fs";
import { isAbsolute, join } from "node:path";

/**
 * The channel a boundary suite publishes its Aiken twin's constants on.
 *
 * These suites *are* the producers for several Aiken constant families: the
 * genuine signed-Cardano boundary they search for is expensive to find and is
 * found here, and the Aiken modules then assert against its bytes. Until #588
 * the only way those bytes reached Aiken was a human copying them, which is how
 * `native-tx-v1.test.ak`'s C20-6/C20-7 constants and
 * `native-tx.max-redeemers.test.ak`'s whole constant block came to be
 * hand-mirrored.
 *
 * Two modes, deliberately different in kind:
 *
 *   * `MIDGARD_PRINT_AIKEN_VECTOR=1` prints the vector for a human reading a
 *     terminal. This is the pre-existing channel and is unchanged.
 *   * `MIDGARD_WRITE_AIKEN_VECTOR=<absolute directory>` writes `<name>.json`
 *     into that directory. This is the machine channel:
 *     `scripts/generate-ordered-collection-boundary-aiken-goldens.mjs` runs these
 *     suites with it set and rebinds the Aiken constants from what it finds.
 *
 * Neither mode changes what the suite asserts. A vector is published *after* the
 * suite has finished checking it against its own pinned expectations, so the
 * generator can never be handed values this suite would itself reject.
 */
export const publishAikenVector = (
  name: string,
  vector: Readonly<Record<string, unknown>>,
): void => {
  if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
    console.info(JSON.stringify(vector, null, 2));
  }
  const directory = process.env.MIDGARD_WRITE_AIKEN_VECTOR;
  if (directory === undefined) {
    return;
  }
  if (!isAbsolute(directory)) {
    throw new Error("MIDGARD_WRITE_AIKEN_VECTOR must be an absolute path");
  }
  if (!/^[a-z0-9-]+$/u.test(name)) {
    throw new Error(`invalid Aiken vector name ${name}`);
  }
  mkdirSync(directory, { recursive: true });
  writeFileSync(
    join(directory, `${name}.json`),
    `${JSON.stringify(vector, null, 2)}\n`,
    "utf8",
  );
};
