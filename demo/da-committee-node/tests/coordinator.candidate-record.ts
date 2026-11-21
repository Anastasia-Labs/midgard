import type { DaAttestationCandidateRecord } from "../src/domain.js";

export const submitted = (txHash: string) => ({
  status: "submitted" as const,
  txHash,
});

export const candidateRecord = ({
  attestationCount,
  status = "initialized",
  outRef = "tx#0",
  bitmap = "00".repeat(32),
}: {
  readonly attestationCount: number;
  readonly status?: DaAttestationCandidateRecord["status"];
  readonly outRef?: string;
  readonly bitmap?: string;
}): DaAttestationCandidateRecord => ({
  deploymentFingerprint: "dep",
  headerHash: "01".repeat(28),
  outRef,
  datumCbor: "d87980",
  attestationCount,
  threshold: 2,
  committeeSignersHash: "02".repeat(32),
  bitmap,
  observedChainPoint: {},
  status,
});
