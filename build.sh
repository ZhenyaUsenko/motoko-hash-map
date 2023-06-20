#!/bin/sh

CANISTER_NAME=$1
ENTRY_POINT=$2

mkdir -p .dfx/local/canisters/${CANISTER_NAME}

$(vessel bin)/moc ${ENTRY_POINT} -o .dfx/local/canisters/${CANISTER_NAME}/${CANISTER_NAME}.wasm \
-c --debug --idl --stable-types --public-metadata candid:service --actor-idl .dfx/local/canisters/idl/ \
--actor-alias ${CANISTER_NAME} $(dfx canister id ${CANISTER_NAME}) $(vessel sources)

ic-wasm .dfx/local/canisters/${CANISTER_NAME}/${CANISTER_NAME}.wasm -o .dfx/local/canisters/${CANISTER_NAME}/${CANISTER_NAME}.wasm shrink

gzip -f -9 .dfx/local/canisters/${CANISTER_NAME}/${CANISTER_NAME}.wasm
