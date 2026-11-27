// Shared status classification for the canonical-V1 evidence verifiers.
//
// Published statuses are routinely decorated with the reason they hold:
// `PASS (structural N/A)`, `PASS (LOCAL_PASS)`,
// `PASS (live-verified on preprod)`. Comparing a whole status cell against the
// literal "PASS" therefore silently excludes exactly the rows whose decoration
// records how the claim was established, so every status-keyed rule written
// that way no-ops on them.
//
// `statusRole` reduces a status to its leading role token and `isPassStatus` is
// the base-role PASS test. Keep status parsing shared between the remaining
// domain reconciliation checks instead of growing subtly different conventions
// in each script.

export const statusRole = (status) =>
  (status ?? "")
    .replace(/[`*]/g, "")
    .trim()
    .split(/[\s(;,]/)[0]
    .toUpperCase();

export const isPassStatus = (status) => statusRole(status) === "PASS";
