import { MATURITY_DURATION_MS } from "@al-ft/midgard-sdk";

/** Finite stage validity, strictly after the committed node's `block_time` and
 * before its merge deadline `block_time + maturity`, the window the capture and
 * terminal checks read. The header's covered end never exceeds `block_time`, so
 * the window also lies after the accused interval. This enforces safety; the
 * workflow must separately reserve inclusion time for all remaining stages.
 * `now` is injectable for chain-clock emulator scenarios. */
export const fabricatedProofValidity = ({
  nodeBlockTime,
  now,
}: {
  /** The state-queue node's `block_time`, never the header's `end_time`. */
  readonly nodeBlockTime: bigint;
  readonly now: number;
}) => {
  if (!Number.isSafeInteger(now) || now < 0 || nodeBlockTime < 0n)
    throw new Error("Invalid fabricated-proof clock or node block time");
  const current = BigInt(now);
  const earliest = nodeBlockTime + 1000n;
  const deadline = nodeBlockTime + MATURITY_DURATION_MS;
  if (current < earliest || current + 1000n >= deadline)
    throw new Error(
      "Fabricated proof has no usable validity window before merge",
    );
  const lower = current - 60_000n > earliest ? current - 60_000n : earliest;
  const upper = current + 120_000n < deadline ? current + 120_000n : deadline;
  if (upper > BigInt(Number.MAX_SAFE_INTEGER))
    throw new Error(
      "Fabricated-proof validity exceeds the clock representation",
    );
  return { validFrom: Number(lower), validTo: Number(upper) };
};
