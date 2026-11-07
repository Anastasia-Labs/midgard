/**
 * `da-bond assemble` in a process whose Ed25519 verifier cannot answer:
 * libsodium fails to initialise. The command refuses with the verifier's
 * typed fault before it reads any witness, and the CLI reports that refusal
 * under the command's name with a non-zero exit code.
 */
import { LedgerSignatureVerifierUnavailableError } from "@al-ft/midgard-core";
import { afterEach, describe, expect, it, vi } from "vitest";

import { failCli } from "../src/commands/cli-runtime.js";
import { daBondAssembleCommand } from "../src/commands/da-bond.js";
import { readDaBondWitnessFile } from "../src/commands/da-bond-files.js";

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    const failed = Promise.reject(new Error("injected initialisation failure"));
    failed.catch(() => undefined);
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? failed
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const CONTEXT = { network: "Preprod", manifestId: "manifest" } as const;

vi.mock("../src/commands/da-bond-files.js", async (importOriginal) => ({
  ...(await importOriginal<object>()),
  readDaBondUnsignedFile: vi.fn(() => Promise.resolve(CONTEXT)),
  readDaBondWitnessFile: vi.fn(() =>
    Promise.reject(new Error("no witness is read")),
  ),
}));

afterEach(() => {
  process.exitCode = undefined;
  vi.restoreAllMocks();
});

describe("da-bond assemble whose Ed25519 verifier cannot answer", () => {
  it("refuses with the verifier's fault before reading a witness, reported by the CLI", async () => {
    const refusal: unknown = await daBondAssembleCommand(
      CONTEXT as never,
      "unsigned.json",
      ["witness.json"],
    ).then(
      () => undefined,
      (error: unknown) => error,
    );
    expect(refusal).toBeInstanceOf(LedgerSignatureVerifierUnavailableError);
    expect(readDaBondWitnessFile).not.toHaveBeenCalled();

    const stderr = vi.spyOn(console, "error").mockImplementation(() => {});
    failCli("da-bond assemble", refusal);
    expect(stderr).toHaveBeenCalledWith(
      expect.stringMatching(
        /^da-bond assemble: .*Ed25519 verification is unavailable: injected initialisation failure/u,
      ),
    );
    expect(process.exitCode).toBe(1);
  });
});
