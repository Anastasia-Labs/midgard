import type {
  MidgardRedeemerItemProofControl,
  MidgardRedeemerItemProofWitness,
} from "@al-ft/midgard-core";

export const redeemerItemExecutor = (
  control: MidgardRedeemerItemProofControl,
  witness: MidgardRedeemerItemProofWitness,
): { readonly family: number; readonly index: number } => {
  const action = witness.action;
  if (action.kind === "openHeader") return { family: 2, index: 2 };
  if (action.kind === "openTail") return { family: 3, index: 3 };
  if (action.kind === "finishData") return { family: 8, index: 16 };
  const inner = action.action;
  if (inner === null) {
    const stage = control.traversal?.stage;
    if (stage === undefined || stage < 1 || stage > 5)
      throw new Error("item NoAction is outside its stage domain");
    return { family: 7, index: stage + 10 };
  }
  switch (inner.kind) {
    case "foldMap":
      return { family: 0, index: 0 };
    case "finalizeFrame":
      return { family: 1, index: 1 };
    case "headScalar":
      return { family: 4, index: 4 };
    case "headSequence":
      return { family: 4, index: 5 };
    case "headMap":
      return { family: 4, index: 6 };
    case "headLargeConstructor":
      return { family: 4, index: 7 };
    case "attachScalar": {
      const stage = control.traversal?.stage;
      if (stage !== 1 && stage !== 2)
        throw new Error("item attachment is outside its stage domain");
      return { family: 5, index: stage + 7 };
    }
    case "foldList":
      return { family: 6, index: 10 };
  }
};
