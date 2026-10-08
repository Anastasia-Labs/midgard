package main

import (
	"errors"
	"fmt"

	pcommon "github.com/blinklabs-io/gouroboros/protocol/common"
	"github.com/fxamacker/cbor/v2"
)

// wirePoint is a chain point: [] for the origin, [slot, hash32] otherwise.
type wirePoint struct {
	origin bool
	slot   uint64
	hash   []byte
}

func originPoint() wirePoint { return wirePoint{origin: true} }

func pointFromCommon(p pcommon.Point) wirePoint {
	if len(p.Hash) == 0 {
		return originPoint()
	}
	return wirePoint{slot: p.Slot, hash: append([]byte(nil), p.Hash...)}
}

func (p wirePoint) common() pcommon.Point {
	if p.origin {
		return pcommon.NewPointOrigin()
	}
	return pcommon.NewPoint(p.slot, p.hash)
}

func (p wirePoint) equal(other wirePoint) bool {
	if p.origin || other.origin {
		return p.origin == other.origin
	}
	return p.slot == other.slot && string(p.hash) == string(other.hash)
}

func (p wirePoint) MarshalCBOR() ([]byte, error) {
	if p.origin {
		return headerEncMode.Marshal([]any{})
	}
	return headerEncMode.Marshal([]any{p.slot, p.hash})
}

func (p *wirePoint) UnmarshalCBOR(data []byte) error {
	var parts []cbor.RawMessage
	if err := headerDecMode.Unmarshal(data, &parts); err != nil {
		return errors.New("point is not an array")
	}
	switch len(parts) {
	case 0:
		*p = originPoint()
		return nil
	case 2:
		var slot uint64
		var hash []byte
		if err := headerDecMode.Unmarshal(parts[0], &slot); err != nil {
			return errors.New("point slot is not an unsigned integer")
		}
		if err := headerDecMode.Unmarshal(parts[1], &hash); err != nil || len(hash) != 32 {
			return errors.New("point hash is not 32 bytes")
		}
		*p = wirePoint{slot: slot, hash: hash}
		return nil
	default:
		return errors.New("point has an invalid arity")
	}
}

// wireTip is the node's tip: [point, blockNo].
type wireTip struct {
	point   wirePoint
	blockNo uint64
}

func (t wireTip) MarshalCBOR() ([]byte, error) {
	return headerEncMode.Marshal([]any{t.point, t.blockNo})
}

// Inbound headers. Every field a type admits is listed; any other key is
// refused by the decoder. Required fields are checked by validate().

type helloHeader struct {
	Type              string  `cbor:"type"`
	Version           *uint64 `cbor:"version"`
	SocketPath        string  `cbor:"socketPath"`
	NetworkMagic      *uint64 `cbor:"networkMagic"`
	RequestDeadlineMs *uint64 `cbor:"requestDeadlineMs"`
}

type csOpenHeader struct {
	Type     string      `cbor:"type"`
	ID       uint64      `cbor:"id"`
	Stream   uint64      `cbor:"stream"`
	Points   []wirePoint `cbor:"points"`
	StartSeq *uint64     `cbor:"startSeq"`
	AckedSeq *uint64     `cbor:"ackedSeq,omitempty"`
	Window   uint64      `cbor:"window"`
}

type csWindowHeader struct {
	Type   string `cbor:"type"`
	Stream uint64 `cbor:"stream"`
	Window uint64 `cbor:"window"`
}

type csAckHeader struct {
	Type   string `cbor:"type"`
	Stream uint64 `cbor:"stream"`
	Seq    uint64 `cbor:"seq"`
}

type csCloseHeader struct {
	Type   string `cbor:"type"`
	ID     uint64 `cbor:"id"`
	Stream uint64 `cbor:"stream"`
}

type lsqAcquireHeader struct {
	Type  string     `cbor:"type"`
	ID    uint64     `cbor:"id"`
	Point *wirePoint `cbor:"point,omitempty"`
}

type lsqReleaseHeader struct {
	Type string `cbor:"type"`
	ID   uint64 `cbor:"id"`
}

type wireTxIn struct {
	_     struct{} `cbor:",toarray"`
	TxID  []byte
	Index uint64
}

type wireCredential struct {
	_    struct{} `cbor:",toarray"`
	Tag  uint64
	Hash []byte
}

type lsqQueryHeader struct {
	Type        string           `cbor:"type"`
	ID          uint64           `cbor:"id"`
	Query       string           `cbor:"query"`
	Addresses   [][]byte         `cbor:"addresses,omitempty"`
	TxIns       []wireTxIn       `cbor:"txIns,omitempty"`
	Credentials []wireCredential `cbor:"credentials,omitempty"`
}

type submitHeader struct {
	Type string  `cbor:"type"`
	ID   uint64  `cbor:"id"`
	Era  *uint64 `cbor:"era,omitempty"`
}

type monitorHasTxHeader struct {
	Type string `cbor:"type"`
	ID   uint64 `cbor:"id"`
	TxID []byte `cbor:"txId"`
}

type monitorSizesHeader struct {
	Type string `cbor:"type"`
	ID   uint64 `cbor:"id"`
}

// Outbound headers.

type helloOkHeader struct {
	Type                string `cbor:"type"`
	Version             uint64 `cbor:"version"`
	NodeToClientVersion uint64 `cbor:"nodeToClientVersion"`
}

type errorHeader struct {
	Type    string `cbor:"type"`
	ID      uint64 `cbor:"id"`
	Code    string `cbor:"code"`
	Message string `cbor:"message"`
}

type fatalHeader struct {
	Type    string `cbor:"type"`
	Code    string `cbor:"code"`
	Message string `cbor:"message"`
}

type okHeader struct {
	Type string `cbor:"type"`
	ID   uint64 `cbor:"id"`
}

type csOpenedHeader struct {
	Type   string    `cbor:"type"`
	ID     uint64    `cbor:"id"`
	Stream uint64    `cbor:"stream"`
	Point  wirePoint `cbor:"point"`
	Tip    wireTip   `cbor:"tip"`
}

type csIntersectNotFoundHeader struct {
	Type   string  `cbor:"type"`
	ID     uint64  `cbor:"id"`
	Stream uint64  `cbor:"stream"`
	Tip    wireTip `cbor:"tip"`
}

type csRollForwardHeader struct {
	Type      string    `cbor:"type"`
	Stream    uint64    `cbor:"stream"`
	Seq       uint64    `cbor:"seq"`
	Point     wirePoint `cbor:"point"`
	BlockNo   uint64    `cbor:"blockNo"`
	BlockType uint64    `cbor:"blockType"`
	PrevHash  []byte    `cbor:"prevHash,omitempty"`
	Tip       wireTip   `cbor:"tip"`
}

type csRollBackwardHeader struct {
	Type   string    `cbor:"type"`
	Stream uint64    `cbor:"stream"`
	Seq    uint64    `cbor:"seq"`
	Point  wirePoint `cbor:"point"`
	Tip    wireTip   `cbor:"tip"`
}

type csFailedHeader struct {
	Type    string `cbor:"type"`
	Stream  uint64 `cbor:"stream"`
	Code    string `cbor:"code"`
	Message string `cbor:"message"`
}

type idOnlyHeader struct {
	Type string `cbor:"type"`
	ID   uint64 `cbor:"id"`
}

type monitorHasTxResultHeader struct {
	Type string `cbor:"type"`
	ID   uint64 `cbor:"id"`
	Has  bool   `cbor:"has"`
}

type monitorSizesResultHeader struct {
	Type     string `cbor:"type"`
	ID       uint64 `cbor:"id"`
	Capacity uint64 `cbor:"capacity"`
	Size     uint64 `cbor:"size"`
	TxCount  uint64 `cbor:"txCount"`
}

// requestError is a refusal of one request, framed as an error answer.
type requestError struct {
	code    string
	message string
}

func (e *requestError) Error() string { return fmt.Sprintf("%s: %s", e.code, e.message) }

func refuse(code, format string, args ...any) *requestError {
	return &requestError{code: code, message: fmt.Sprintf(format, args...)}
}
