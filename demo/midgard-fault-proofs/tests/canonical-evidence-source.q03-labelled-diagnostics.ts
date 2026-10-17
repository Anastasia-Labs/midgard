import "./canonical-evidence-source.q03-canonical-evidence-builders.js";

import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { main as cliMain } from "../src/bin.js";
import {
  diagnosticBlockTransactionsFromMidgardNode,
  diagnosticEvidenceBanner,
} from "../src/evidence/index.js";
import { doubleSpendBlock } from "./canonical-evidence-source.q03-provenance-admission.js";

describe("Q03 labelled diagnostics", () => {
  it("rejects diagnostic grade on every prepare CLI verb before proof construction", async () => {
    const priorArgv = process.argv;
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    const cases = [
      ["prepare-double-spend", "--header-hash", "11".repeat(28)],
      [
        "prepare-invalid-range",
        "--header-hash",
        "11".repeat(28),
        "--block-slot",
        "10",
      ],
      ["prepare-non-existent-input", "--header-hash", "11".repeat(28)],
      [
        "prepare-zero-input",
        "--header-hash",
        "11".repeat(28),
        "--expected-transactions-root",
        "22".repeat(32),
      ],
    ];
    try {
      for (const commandArgs of cases) {
        process.argv = [
          "node",
          "midgard-fault-proofs",
          ...commandArgs,
          "--midgard-node-url",
          "http://operator.invalid",
        ];
        await expect(cliMain()).rejects.toThrow(/prohibited_trust_class/u);
      }
      expect(stderr).toHaveBeenCalledTimes(4);
      for (const call of stderr.mock.calls) {
        expect(call[0]).toMatch(/^DIAGNOSTIC EVIDENCE/u);
      }
    } finally {
      process.argv = priorArgv;
      stderr.mockRestore();
    }
  });

  it("labels operator REST imports and keeps them out of security paths", async () => {
    const fixture = await doubleSpendBlock();
    const payloads = fixture.transactions.map((tx) => ({
      nodeTxId: tx.txId,
      txCbor: tx.canonicalCbor.toString("hex"),
    }));
    const fetchImpl = (input: string | URL): Promise<Response> => {
      const url = String(input);
      if (url.includes("/block")) {
        return Promise.resolve(
          new Response(
            JSON.stringify({ hashes: payloads.map((tx) => tx.nodeTxId) }),
          ),
        );
      }
      const txId = new URL(url).searchParams.get("tx_hash");
      const payload = payloads.find((tx) => tx.nodeTxId === txId);
      return Promise.resolve(
        new Response(JSON.stringify({ tx: payload?.txCbor })),
      );
    };

    const diagnostic = await diagnosticBlockTransactionsFromMidgardNode({
      midgardNodeUrl: "http://operator.invalid",
      headerHash: fixture.headerHash,
      fetchImpl,
    });

    expect(diagnostic.provenance.grade).toBe("diagnostic");
    expect(diagnostic.provenance.trustClass).toBe("operator_admin_api");
    expect(diagnostic.provenance.diagnosticLabel).toMatch(/never a security/u);
    expect(diagnostic.transactions).toHaveLength(3);
    expect(diagnosticEvidenceBanner(diagnostic.provenance)).toMatch(
      /^DIAGNOSTIC EVIDENCE \(operator_admin_api\/midgard-node-url\)/u,
    );
    expect(() =>
      SDK.assertSecurityGradeEvidence(diagnostic.provenance),
    ).toThrowError(/prohibited_trust_class/u);
  });
});
