import { MATURITY_DURATION_MS } from "@al-ft/midgard-sdk";

/** Finite stage validity, strictly after the accused interval and before merge.
 * This enforces safety; the workflow must separately reserve inclusion time for
 * all remaining stages. `now` is injectable for chain-clock emulator scenarios. */
export const fabricatedProofValidity = (headerEnd: bigint, now: number) => {
  if (!Number.isSafeInteger(now) || now < 0 || headerEnd < 0n)
    throw new Error("Invalid fabricated-proof clock or header end");
  const current = BigInt(now);
  const deadline = headerEnd + MATURITY_DURATION_MS;
  if (current < headerEnd + 1000n || current + 1000n >= deadline)
    throw new Error(
      "Fabricated proof has no usable validity window before merge",
    );
  const lower =
    current - 60_000n > headerEnd + 1000n
      ? current - 60_000n
      : headerEnd + 1000n;
  const upper = current + 120_000n < deadline ? current + 120_000n : deadline;
  if (upper > BigInt(Number.MAX_SAFE_INTEGER))
    throw new Error(
      "Fabricated-proof validity exceeds the clock representation",
    );
  return { validFrom: Number(lower), validTo: Number(upper) };
};
