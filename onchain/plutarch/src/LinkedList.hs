{- |
Module      : LinkedList
Description : Plutarch port of the element-authentication core of
              @aiken-design-patterns/linked-list.ak@ (v1.7.0).

An on-chain linked list is a set of UTxOs each authenticated by an NFT under one
policy: a root, and nodes whose asset names are @node_key_prefix ++ node_key@.
Every element's datum is a "LinkedList.Types".'LinkedList.Types.PElement',
pairing the structural role with a link to the successor.

This module covers everything Midgard uses from the library: the authentication
and reading layer, and the structural operations built on it — @init@, @deinit@,
@insert_ascending@, @insert_descending@, @append_unordered@, @remove@,
@fold_from_root@, and the two spend gates.

Not ported, because Midgard does not use them: @prepend_unordered@, and the
@linked_list/advanced@ and @linked_list/nested@ variant modules.

Continuations are written as Haskell functions taking Plutarch terms, which is
the natural rendering of Aiken's @let a, b <- f(...)@ backpassing. They erase at
compile time, so this costs nothing on-chain.
-}
module LinkedList (
  -- * Element authentication and reading
  pauthenticateElementUtxoAndGetInfo,
  pgetElementInfo,
  pgetRootElementInfo,
  pgetNodeElementInfo,

  -- * Authentic-input readers
  pvalidateSingularAuthenticInput,
  pvalidateDualAuthenticInputs,

  -- * Initialization
  pinit,
  pdeinit,

  -- * Element addition and removal
  pinsertAscending,
  pinsertDescending,
  pappendUnordered,
  premove,

  -- * Folding
  pfoldFromRoot,

  -- * Spend gates
  pspendForAddingOrRemovingAnElement,
  pspendForUpdatingElementsData,
) where

import Data.Kind (Type)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCurrencySymbol,
  PMintValue,
  PScriptHash,
  PTokenName,
  PTxInInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import LinkedList.Internal (
  POrdering (..),
  pauthenticateElementUtxoAndGetInfoHelper,
  pisOnlyMintUnderPolicy,
  pkeyFitsBetween,
  pvalidateDualAuthenticInputsHelper,
  pvalidateSingularAuthenticInputHelper,
 )
import LinkedList.Types (
  ElementEval,
  NodeEval,
  PLovelaceChange,
  RootEval,
  PElement (..),
  PElementData (..),
  PLink,
  PNodeKeyPrefix,
  PNodeKeyPrefixLength,
  PRootKey,
 )

{- | Aiken @linked_list.authenticate_element_utxo_and_get_info@.

Authenticates the UTxO as an inline-datum singleton list element under
@nft_policy_id@, decodes its datum as a @GenericElement@, and hands the
continuation the address, Lovelace, NFT asset name, structural element data,
link, and optional reference script.

This is the shared base of every reader below. It says nothing about /which/
element this is — the asset name is returned raw, and matching it against the
root key or the node namespace is the callers' job.
-}
pauthenticateElementUtxoAndGetInfo ::
  forall (s :: S) (r :: S -> Type).
  Term s PTxOut ->
  Term s (PAsData PCurrencySymbol) ->
  ( Term s PAddress ->
    Term s PInteger ->
    Term s (PAsData PTokenName) ->
    Term s PElementData ->
    Term s PLink ->
    Term s (PMaybeData PScriptHash) ->
    Term s r
  ) ->
  Term s r
pauthenticateElementUtxoAndGetInfo elementUtxo nftPolicyId k =
  pauthenticateElementUtxoAndGetInfoHelper elementUtxo nftPolicyId $
    \address lovelace nftName datumData refScript -> P.do
      -- `expect Element { data, link }: GenericElement = utxo_datum_data`
      PElement {pelement'data, pelement'link} <-
        pmatch $ pfromData (punsafeCoerce @(PAsData PElement) datumData)
      k address lovelace nftName (pfromData pelement'data) pelement'link refScript

{- | Aiken @linked_list.get_element_info@.

Authenticates a root /or/ node UTxO and passes its info on. The key handed to
the continuation is 'PDNothing' for the root and @'PDJust' node_key@ for a node,
with the prefix stripped.

The branch is the security-relevant part: a @Root@ datum forces the asset name
to equal the root key, and a @Node@ datum forces it to start with the node
prefix. Neither can masquerade as the other.

Returns an 'ElementEval', so the caller supplies policy and namespace once at
the end.
-}
pgetElementInfo ::
  forall (s :: S) (r :: S -> Type).
  Term s PTxOut ->
  ( Term s PAddress ->
    Term s PInteger ->
    Term s (PMaybeData PByteString) ->
    Term s PData ->
    Term s PLink ->
    Term s r
  ) ->
  ElementEval s r
pgetElementInfo elementUtxo infoValidations =
  \listNftPolicyId rootKey nodeKeyPrefix nodeKeyPrefixLength ->
    pauthenticateElementUtxoAndGetInfo elementUtxo listNftPolicyId $
      \address lovelace assetName elementData link _refScript ->
        pmatch elementData $ \case
          PRoot {pelementData'root} ->
            pif
              (assetName #== rootKey)
              (infoValidations address lovelace (pcon (PDNothing)) pelementData'root link)
              perror
          PNode {pelementData'node} -> P.do
            nameBytes <- plet $ pto (pfromData assetName)
            pif
              (psliceBS # 0 # nodeKeyPrefixLength # nameBytes #== nodeKeyPrefix)
              ( infoValidations
                  address
                  lovelace
                  ( pcon
                      ( PDJust
                          ( pdata
                              ( psliceBS
                                  # nodeKeyPrefixLength
                                  # (plengthBS # nameBytes - nodeKeyPrefixLength)
                                  # nameBytes
                              )
                          )
                      )
                  )
                  pelementData'node
                  link
              )
              perror

{- | Aiken @linked_list.get_root_element_info@.

Accepts only the root, rejecting node UTxOs immediately. Returns a 'RootEval' —
no node namespace is needed, so root-only branches need not know the prefix.
-}
pgetRootElementInfo ::
  forall (s :: S) (r :: S -> Type).
  Term s PTxOut ->
  ( Term s PAddress ->
    Term s PInteger ->
    Term s PData ->
    Term s PLink ->
    Term s r
  ) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PRootKey) ->
  Term s r
pgetRootElementInfo elementUtxo infoValidations listNftPolicyId rootKey =
  pauthenticateElementUtxoAndGetInfo elementUtxo listNftPolicyId $
    \address lovelace assetName elementData link _refScript ->
      pif
        (assetName #== rootKey)
        ( pmatch elementData $ \case
            PRoot {pelementData'root} ->
              infoValidations address lovelace pelementData'root link
            PNode _ -> perror
        )
        perror

{- | Aiken @linked_list.get_node_element_info@.

Accepts only a node, rejecting the root. Returns a 'NodeEval' — the root key is
irrelevant here, so node-only branches need not know it.
-}
pgetNodeElementInfo ::
  forall (s :: S) (r :: S -> Type).
  Term s PTxOut ->
  ( Term s PAddress ->
    Term s PInteger ->
    Term s PByteString ->
    Term s PData ->
    Term s PLink ->
    Term s r
  ) ->
  NodeEval s r
pgetNodeElementInfo elementUtxo infoValidations =
  \listNftPolicyId nodeKeyPrefix nodeKeyPrefixLength ->
    pauthenticateElementUtxoAndGetInfo elementUtxo listNftPolicyId $
      \address lovelace assetName elementData link _refScript ->
        pmatch elementData $ \case
          PRoot _ -> perror
          PNode {pelementData'node} -> P.do
            nameBytes <- plet $ pto (pfromData assetName)
            pif
              (psliceBS # 0 # nodeKeyPrefixLength # nameBytes #== nodeKeyPrefix)
              ( infoValidations
                  address
                  lovelace
                  ( psliceBS
                      # nodeKeyPrefixLength
                      # (plengthBS # nameBytes - nodeKeyPrefixLength)
                      # nameBytes
                  )
                  pelementData'node
                  link
              )
              perror

{- | Aiken @linked_list.spend_for_adding_or_removing_an_element@.

@
!(assets.tokens(tx_mint, list_nft_policy_id) |> dict.is_empty)
@

A gate, not an authorisation. It permits a structural list spend only when the
list policy mints or burns something in the same transaction; the minting policy
is what proves the transition is a legitimate one. Using this on its own to
justify moving a list UTxO would be a mistake, and the Aiken docs say so
explicitly.
-}
pspendForAddingOrRemovingAnElement ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol :--> PMintValue :--> PBool)
pspendForAddingOrRemovingAnElement = phoistAcyclic $
  plam $ \listNftPolicyId txMint ->
    pmatch (AssocMap.plookup # pfromData listNftPolicyId # pto (pto txMint)) $ \case
      PNothing -> pconstant False
      PJust nameQtyMap -> pnot # (pnull # pto (pto nameQtyMap))

--------------------------------------------------------------------------------
-- Authentic-input readers
--------------------------------------------------------------------------------

-- | Aiken @linked_list.validate_singular_authentic_input@.
pvalidateSingularAuthenticInput ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PInteger ->
    Term s (PAsData PTokenName) ->
    Term s PElementData ->
    Term s PLink ->
    Term s PBool
  ) ->
  Term s PBool
pvalidateSingularAuthenticInput inputs nftPolicyId return_ =
  pvalidateSingularAuthenticInputHelper inputs nftPolicyId $
    \input lovelace assetName datumData -> P.do
      PElement {pelement'data, pelement'link} <- pmatch $ pdecodeElement datumData
      return_ input lovelace assetName (pfromData pelement'data) pelement'link

-- | Aiken @linked_list.validate_dual_authentic_inputs@.
pvalidateDualAuthenticInputs ::
  forall (s :: S).
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PAsData PCurrencySymbol) ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PInteger ->
    Term s (PAsData PTokenName) ->
    Term s PElementData ->
    Term s PLink ->
    Term s (PAsData PTxInInfo) ->
    Term s PInteger ->
    Term s (PAsData PTokenName) ->
    Term s PElementData ->
    Term s PLink ->
    Term s PBool
  ) ->
  Term s PBool
pvalidateDualAuthenticInputs anchorOutref inputs nftPolicyId with_ =
  pvalidateDualAuthenticInputsHelper anchorOutref inputs nftPolicyId $
    \aIn aLov aName aDatum bIn bLov bName bDatum -> P.do
      PElement {pelement'data = aData, pelement'link = aLink} <- pmatch $ pdecodeElement aDatum
      PElement {pelement'data = bData, pelement'link = bLink} <- pmatch $ pdecodeElement bDatum
      with_
        aIn
        aLov
        aName
        (pfromData aData)
        aLink
        bIn
        bLov
        bName
        (pfromData bData)
        bLink

-- | @expect Element { data, link }: GenericElement = datum_data@.
pdecodeElement :: forall (s :: S). Term s PData -> Term s PElement
pdecodeElement d = pfromData (punsafeCoerce @(PAsData PElement) d)

--------------------------------------------------------------------------------
-- Initialization and de-initialization
--------------------------------------------------------------------------------

{- | Aiken @linked_list.init@.

Creates the root UTxO. @nonce_validated@ must carry proof of a consumed one-time
nonce; the Aiken docs warn that passing a literal @True@ is only appropriate in
tests, and the same applies here — nothing in this function makes the root
unique on its own.

A new list starts empty, so the root's link must be absent.
-}
pinit ::
  forall (s :: S).
  Term s PBool ->
  Term s PTxOut ->
  Term s PMintValue ->
  (Term s PAddress -> Term s PInteger -> Term s PData -> Term s PBool) ->
  RootEval s PBool
pinit nonceValidated producedElementOutput txMint rootValidator =
  \listNftPolicyId rootKey ->
    pif
      nonceValidated
      ( pauthenticateElementUtxoAndGetInfo producedElementOutput listNftPolicyId $
          \address lovelace assetName elementData link _refScript ->
            pmatch elementData $ \case
              PNode _ -> perror
              PRoot {pelementData'root} ->
                pand'List
                  [ assetName #== rootKey
                  , pisOnlyMintUnderPolicy # txMint # listNftPolicyId # assetName # 1
                  , pisNothing link
                  , rootValidator address lovelace pelementData'root
                  ]
      )
      perror

{- | Aiken @linked_list.deinit@.

Tears down an /empty/ list: exactly one authentic input, it is the root, its
link is absent, and the transaction burns exactly the root NFT.
-}
pdeinit ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  (Term s (PAsData PTxInInfo) -> Term s PInteger -> Term s PData -> Term s PBool) ->
  RootEval s PBool
pdeinit inputs txMint rootValidator =
  \listNftPolicyId rootKey ->
    pvalidateSingularAuthenticInput inputs listNftPolicyId $
      \elementInput lovelace assetName elementData link ->
        pmatch elementData $ \case
          PNode _ -> perror
          PRoot {pelementData'root} ->
            pand'List
              [ pisOnlyMintUnderPolicy # txMint # listNftPolicyId # assetName # (-1)
              , assetName #== rootKey
              , pisNothing link
              , rootValidator elementInput lovelace pelementData'root
              ]

--------------------------------------------------------------------------------
-- Element addition
--------------------------------------------------------------------------------

{- | Aiken @linked_list.common_insertion_validations@.

The shared pre-check behind every insertion: exactly one authentic anchor input,
an authentic continued anchor output and new node output, the anchor's address
and NFT and data preserved, the new node inside the node namespace and sharing
the anchor's payment credential, exactly the new node NFT minted, and the links
rewired anchor → new node → anchor's previous successor.

Preserving the anchor's data is what makes this the /default/ module; Aiken's
@linked_list/advanced@ is where caller-validated anchor data changes live.
-}
pcommonInsertionValidations ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol) ->
  Term s PNodeKeyPrefix ->
  Term s PNodeKeyPrefixLength ->
  Term s PTxOut ->
  Term s PTxOut ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PLovelaceChange ->
    Term s (PAsData PTokenName) ->
    Term s PElementData ->
    Term s PLink ->
    Term s PInteger ->
    Term s PByteString ->
    Term s PData ->
    Term s PLink ->
    Term s PBool
  ) ->
  Term s PBool
pcommonInsertionValidations
  listNftPolicyId
  nodeKeyPrefix
  nodeKeyPrefixLength
  continuedAnchorOutput
  newNodeOutput
  inputs
  txMint
  callback =
    pvalidateSingularAuthenticInput inputs listNftPolicyId $
      \anchorInput anchorLovelace anchorAssetName anchorData anchorLink -> P.do
        PTxInInfo {ptxInInfo'resolved = anchorResolved} <- pmatch $ pfromData anchorInput
        anchorAddress <- plet $ ptxOutAddress anchorResolved
        PAddress {paddress'credential = anchorCred} <- pmatch anchorAddress
        pauthenticateElementUtxoAndGetInfo continuedAnchorOutput listNftPolicyId $
          \contAddress contLovelace contAssetName contData contLink _ ->
            pauthenticateElementUtxoAndGetInfo newNodeOutput listNftPolicyId $
              \newNodeAddress newNodeLovelace newNodeAssetName newNodeElementData newNodeLink _ -> P.do
                PAddress {paddress'credential = newNodeCred} <- pmatch newNodeAddress
                newNodeNameBytes <- plet $ pto (pfromData newNodeAssetName)
                newNodeKey <-
                  plet $ pdropBS # nodeKeyPrefixLength # newNodeNameBytes
                newNodeData <-
                  plet $ pmatch newNodeElementData $ \case
                    PRoot _ -> perror
                    PNode {pelementData'node} -> pelementData'node
                pand'List
                  [ anchorAddress #== contAddress
                  , anchorCred #== newNodeCred
                  , anchorAssetName #== contAssetName
                  , anchorData #== contData
                  , ptakeBS # nodeKeyPrefixLength # newNodeNameBytes #== nodeKeyPrefix
                  , pisOnlyMintUnderPolicy # txMint # listNftPolicyId # newNodeAssetName # 1
                  , contLink #== pjustBS newNodeKey
                  , newNodeLink #== anchorLink
                  , callback
                      anchorInput
                      (contLovelace - anchorLovelace)
                      anchorAssetName
                      anchorData
                      anchorLink
                      newNodeLovelace
                      newNodeKey
                      newNodeData
                      newNodeLink
                  ]

{- | Aiken @linked_list.insert_ordered@ — shared by the two ordered inserts.

Ordering rule, with @required@ being 'PLess' for ascending and 'PGreater' for
descending:

  * root anchor: @new@ ordered against the old first node;
  * node anchor: @anchor@, @new@, and the anchor's old successor in order.
-}
pinsertOrdered ::
  forall (s :: S).
  Term s POrdering ->
  Term s PTxOut ->
  Term s PTxOut ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PLovelaceChange ->
    Term s (PMaybeData PByteString) ->
    Term s PData ->
    Term s PInteger ->
    Term s PByteString ->
    Term s PData ->
    Term s PLink ->
    Term s PBool
  ) ->
  ElementEval s PBool
pinsertOrdered requiredOrdering contAnchorOutput newNodeOutput inputs txMint additional =
  \listNftPolicyId rootKey nodeKeyPrefix nodeKeyPrefixLength ->
    pcommonInsertionValidations
      listNftPolicyId
      nodeKeyPrefix
      nodeKeyPrefixLength
      contAnchorOutput
      newNodeOutput
      inputs
      txMint
      $ \anchorInput anchorLovelaceChange anchorAssetName anchorData anchorLink newNodeLovelace newNodeKey newNodeData newNodeLink ->
        pmatch anchorData $ \case
          PRoot {pelementData'root} ->
            pand'List
              [ anchorAssetName #== rootKey
              , pkeyFitsBetween # requiredOrdering # pcon PDNothing # newNodeKey # anchorLink
              , additional
                  anchorInput
                  anchorLovelaceChange
                  (pcon PDNothing)
                  pelementData'root
                  newNodeLovelace
                  newNodeKey
                  newNodeData
                  newNodeLink
              ]
          PNode {pelementData'node} -> P.do
            nameBytes <- plet $ pto (pfromData anchorAssetName)
            anchorNodeKey <- plet $ pdropBS # nodeKeyPrefixLength # nameBytes
            pand'List
              [ ptakeBS # nodeKeyPrefixLength # nameBytes #== nodeKeyPrefix
              , pkeyFitsBetween # requiredOrdering # pjustBS anchorNodeKey # newNodeKey # anchorLink
              , additional
                  anchorInput
                  anchorLovelaceChange
                  (pjustBS anchorNodeKey)
                  pelementData'node
                  newNodeLovelace
                  newNodeKey
                  newNodeData
                  newNodeLink
              ]

-- | Aiken @linked_list.insert_ascending@ — @new < next@ throughout.
pinsertAscending ::
  forall (s :: S).
  Term s PTxOut ->
  Term s PTxOut ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PLovelaceChange ->
    Term s (PMaybeData PByteString) ->
    Term s PData ->
    Term s PInteger ->
    Term s PByteString ->
    Term s PData ->
    Term s PLink ->
    Term s PBool
  ) ->
  ElementEval s PBool
pinsertAscending = pinsertOrdered (pcon PLess)

-- | Aiken @linked_list.insert_descending@ — @new > next@ throughout.
pinsertDescending ::
  forall (s :: S).
  Term s PTxOut ->
  Term s PTxOut ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PLovelaceChange ->
    Term s (PMaybeData PByteString) ->
    Term s PData ->
    Term s PInteger ->
    Term s PByteString ->
    Term s PData ->
    Term s PLink ->
    Term s PBool
  ) ->
  ElementEval s PBool
pinsertDescending = pinsertOrdered (pcon PGreater)

{- | Aiken @linked_list.append_unordered@.

Appends a new terminal node. Both the anchor and the new node must be terminal;
there is no key comparison, so application-level uniqueness is the caller's
responsibility.
-}
pappendUnordered ::
  forall (s :: S).
  Term s PTxOut ->
  Term s PTxOut ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PLovelaceChange ->
    Term s (PMaybeData PByteString) ->
    Term s PData ->
    Term s PInteger ->
    Term s PByteString ->
    Term s PData ->
    Term s PBool
  ) ->
  ElementEval s PBool
pappendUnordered contAnchorOutput newNodeOutput inputs txMint additional =
  \listNftPolicyId rootKey nodeKeyPrefix nodeKeyPrefixLength ->
    pcommonInsertionValidations
      listNftPolicyId
      nodeKeyPrefix
      nodeKeyPrefixLength
      contAnchorOutput
      newNodeOutput
      inputs
      txMint
      $ \anchorInput anchorLovelaceChange anchorAssetName anchorData anchorLink newNodeLovelace newNodeKey newNodeData newNodeLink ->
        pand'List
          [ pisNothing anchorLink
          , pisNothing newNodeLink
          , pmatch anchorData $ \case
              PRoot {pelementData'root} ->
                pand'List
                  [ anchorAssetName #== rootKey
                  , additional
                      anchorInput
                      anchorLovelaceChange
                      (pcon PDNothing)
                      pelementData'root
                      newNodeLovelace
                      newNodeKey
                      newNodeData
                  ]
              PNode {pelementData'node} -> P.do
                nameBytes <- plet $ pto (pfromData anchorAssetName)
                pand'List
                  [ ptakeBS # nodeKeyPrefixLength # nameBytes #== nodeKeyPrefix
                  , additional
                      anchorInput
                      anchorLovelaceChange
                      (pjustBS (pdropBS # nodeKeyPrefixLength # nameBytes))
                      pelementData'node
                      newNodeLovelace
                      newNodeKey
                      newNodeData
                  ]
          ]

--------------------------------------------------------------------------------
-- Element removal and folding
--------------------------------------------------------------------------------

{- | Aiken @linked_list.remove@.

Removes the node the anchor points at. Two authentic inputs are required — the
anchor and the node being removed — with the anchor selected by output
reference. The continued anchor preserves address, NFT and data, and adopts the
removed node's link; the removed node's NFT is burnt.
-}
premove ::
  forall (s :: S).
  Term s PTxOutRef ->
  Term s PTxOut ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PLovelaceChange ->
    Term s (PMaybeData PByteString) ->
    Term s PData ->
    Term s (PAsData PTxInInfo) ->
    Term s PInteger ->
    Term s PByteString ->
    Term s PData ->
    Term s PLink ->
    Term s PBool
  ) ->
  ElementEval s PBool
premove anchorInputOutref contAnchorOutput inputs txMint additional =
  \listNftPolicyId rootKey nodeKeyPrefix nodeKeyPrefixLength ->
    pvalidateDualAuthenticInputs anchorInputOutref inputs listNftPolicyId $
      \anchorInput anchorLovelace anchorAssetName anchorData anchorLink removingInput removingLovelace removingAssetName removingElementData removingLink -> P.do
        PTxInInfo {ptxInInfo'resolved = anchorResolved} <- pmatch $ pfromData anchorInput
        anchorAddress <- plet $ ptxOutAddress anchorResolved
        pauthenticateElementUtxoAndGetInfo contAnchorOutput listNftPolicyId $
          \contAddress contLovelace contAssetName contData contLink _ -> P.do
            removingNameBytes <- plet $ pto (pfromData removingAssetName)
            removingKey <- plet $ pdropBS # nodeKeyPrefixLength # removingNameBytes
            removingData <-
              plet $ pmatch removingElementData $ \case
                PRoot _ -> perror
                PNode {pelementData'node} -> pelementData'node
            pand'List
              [ ptakeBS # nodeKeyPrefixLength # removingNameBytes #== nodeKeyPrefix
              , contAddress #== anchorAddress
              , contAssetName #== anchorAssetName
              , contData #== anchorData
              , pisOnlyMintUnderPolicy # txMint # listNftPolicyId # removingAssetName # (-1)
              , anchorLink #== pjustBS removingKey
              , contLink #== removingLink
              , pmatch anchorData $ \case
                  PRoot {pelementData'root} ->
                    pand'List
                      [ anchorAssetName #== rootKey
                      , additional
                          anchorInput
                          (contLovelace - anchorLovelace)
                          (pcon PDNothing)
                          pelementData'root
                          removingInput
                          removingLovelace
                          removingKey
                          removingData
                          removingLink
                      ]
                  PNode {pelementData'node} -> P.do
                    anchorNameBytes <- plet $ pto (pfromData anchorAssetName)
                    pand'List
                      [ ptakeBS # nodeKeyPrefixLength # anchorNameBytes #== nodeKeyPrefix
                      , additional
                          anchorInput
                          (contLovelace - anchorLovelace)
                          (pjustBS (pdropBS # nodeKeyPrefixLength # anchorNameBytes))
                          pelementData'node
                          removingInput
                          removingLovelace
                          removingKey
                          removingData
                          removingLink
                      ]
              ]

{- | Aiken @linked_list.fold_from_root@.

Folds the root's first node into the root: the node's NFT is burnt and the root
adopts its link. Unlike 'premove' the anchor must be the root, and the continued
element must still be a root — the caller sees both the old and the new root
payload, which is what makes this a fold rather than a deletion.
-}
pfoldFromRoot ::
  forall (s :: S).
  Term s PTxOutRef ->
  Term s PTxOut ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PMintValue ->
  ( Term s (PAsData PTxInInfo) ->
    Term s PLovelaceChange ->
    Term s PData ->
    Term s (PAsData PTxInInfo) ->
    Term s PInteger ->
    Term s PByteString ->
    Term s PData ->
    Term s PLink ->
    Term s PData ->
    Term s PBool
  ) ->
  ElementEval s PBool
pfoldFromRoot anchorRootInputOutref contRootOutput inputs txMint additional =
  \listNftPolicyId rootKey nodeKeyPrefix nodeKeyPrefixLength ->
    pvalidateDualAuthenticInputs anchorRootInputOutref inputs listNftPolicyId $
      \rootInput rootLovelace rootAssetName rootElementData rootLink foldingInput foldingLovelace foldingAssetName foldingElementData foldingLink -> P.do
        PTxInInfo {ptxInInfo'resolved = rootResolved} <- pmatch $ pfromData rootInput
        rootAddress <- plet $ ptxOutAddress rootResolved
        rootData <-
          plet $ pmatch rootElementData $ \case
            PNode _ -> perror
            PRoot {pelementData'root} -> pelementData'root
        pauthenticateElementUtxoAndGetInfo contRootOutput listNftPolicyId $
          \contAddress contLovelace contAssetName contElementData contLink _ -> P.do
            foldingNameBytes <- plet $ pto (pfromData foldingAssetName)
            foldingKey <- plet $ pdropBS # nodeKeyPrefixLength # foldingNameBytes
            foldingData <-
              plet $ pmatch foldingElementData $ \case
                PRoot _ -> perror
                PNode {pelementData'node} -> pelementData'node
            contRootData <-
              plet $ pmatch contElementData $ \case
                PNode _ -> perror
                PRoot {pelementData'root} -> pelementData'root
            pand'List
              [ ptakeBS # nodeKeyPrefixLength # foldingNameBytes #== nodeKeyPrefix
              , pisOnlyMintUnderPolicy # txMint # listNftPolicyId # foldingAssetName # (-1)
              , rootAddress #== contAddress
              , rootAssetName #== contAssetName
              , rootLink #== pjustBS foldingKey
              , contLink #== foldingLink
              , rootAssetName #== rootKey
              , additional
                  rootInput
                  (contLovelace - rootLovelace)
                  rootData
                  foldingInput
                  foldingLovelace
                  foldingKey
                  foldingData
                  foldingLink
                  contRootData
              ]

--------------------------------------------------------------------------------
-- Non-structural continuation
--------------------------------------------------------------------------------

{- | Aiken @linked_list.spend_for_updating_elements_data@.

Continues one element without changing the list's structure: address, NFT,
constructor and link are all preserved, and the list policy must mint nothing.
That last condition is also what prevents double satisfaction here, which is why
it is passed into the one-to-one indexer rather than checked separately.
-}
pspendForUpdatingElementsData ::
  forall (s :: S).
  Term s PInteger ->
  Term s PInteger ->
  Term s PTxOutRef ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PAsData PTxOut)) ->
  Term s PMintValue ->
  ( Term s PAddress ->
    Term s PLovelaceChange ->
    Term s (PMaybeData PByteString) ->
    Term s PData ->
    Term s PData ->
    Term s PLink ->
    Term s PBool
  ) ->
  ElementEval s PBool
pspendForUpdatingElementsData
  elementInputIndex
  contElementOutputIndex
  elementInputOutref
  inputs
  outputs
  txMint
  additional =
    \listNftPolicyId rootKey nodeKeyPrefix nodeKeyPrefixLength -> P.do
      -- `singular_utxo_indexer.one_to_one`, with the no-list-mint condition as
      -- the double-satisfaction guard.
      noListMint <-
        plet $ pnot # (pspendForAddingOrRemovingAnElement # listNftPolicyId # txMint)
      elementInput <- plet $ pelemAt # elementInputIndex # inputs
      contOutput <- plet $ pfromData (pelemAt # contElementOutputIndex # outputs)
      PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch $ pfromData elementInput
      pif
        (pand' # noListMint # (ptxInInfo'outRef #== elementInputOutref))
        ( pauthenticateElementUtxoAndGetInfo ptxInInfo'resolved listNftPolicyId $
            \address lovelace assetName elementData link _ ->
              pauthenticateElementUtxoAndGetInfo contOutput listNftPolicyId $
                \contAddress contLovelace contAssetName contElementData contLink _ ->
                  pand'List
                    [ address #== contAddress
                    , assetName #== contAssetName
                    , link #== contLink
                    , pmatch elementData $ \case
                        PRoot {pelementData'root} ->
                          pmatch contElementData $ \case
                            PNode _ -> perror
                            PRoot {pelementData'root = contRootData} ->
                              pand'List
                                [ assetName #== rootKey
                                , additional
                                    address
                                    (contLovelace - lovelace)
                                    (pcon PDNothing)
                                    pelementData'root
                                    contRootData
                                    link
                                ]
                        PNode {pelementData'node} ->
                          pmatch contElementData $ \case
                            PRoot _ -> perror
                            PNode {pelementData'node = contNodeData} -> P.do
                              nameBytes <- plet $ pto (pfromData assetName)
                              pand'List
                                [ ptakeBS # nodeKeyPrefixLength # nameBytes #== nodeKeyPrefix
                                , additional
                                    address
                                    (contLovelace - lovelace)
                                    (pjustBS (pdropBS # nodeKeyPrefixLength # nameBytes))
                                    pelementData'node
                                    contNodeData
                                    link
                                ]
                    ]
        )
        perror

--------------------------------------------------------------------------------
-- Small shared helpers
--------------------------------------------------------------------------------

-- | Aiken @bytearray.take@.
ptakeBS :: forall (s :: S). Term s (PInteger :--> PByteString :--> PByteString)
ptakeBS = phoistAcyclic $ plam $ \n bs -> psliceBS # 0 # n # bs

-- | Aiken @bytearray.drop@.
pdropBS :: forall (s :: S). Term s (PInteger :--> PByteString :--> PByteString)
pdropBS = phoistAcyclic $ plam $ \n bs -> psliceBS # n # (plengthBS # bs - n) # bs

-- | @Some key@ as a 'PLink'.
pjustBS :: forall (s :: S). Term s PByteString -> Term s PLink
pjustBS b = pcon (PDJust (pdata b))

-- | Whether a link is terminal.
pisNothing :: forall (s :: S). Term s PLink -> Term s PBool
pisNothing l = pmatch l $ \case
  PDNothing -> pconstant True
  PDJust _ -> pconstant False

-- | The address of a 'PTxOut', without destructuring at every use site.
ptxOutAddress :: forall (s :: S). Term s PTxOut -> Term s PAddress
ptxOutAddress out = pmatch out $ \(PTxOut {ptxOut'address}) -> ptxOut'address
