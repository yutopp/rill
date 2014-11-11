//
// Copyright yutopp 2013 - .
//
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <rill/compile_time/llvm_engine/value_converter.hpp>
#include <rill/config/macros.hpp>

#include <iostream>


namespace rill
{
    namespace compile_time
    {
        namespace llvm_engine
        {
            auto convert_storage_to_generic_value(
                void* const storage,
                llvm::Type const* const to_type
                ) -> llvm::GenericValue
            {
                llvm::GenericValue gv;

                switch( to_type->getTypeID() ) {
//                case llvm::Type::VoidTyID:
//                    break;
//
//                case llvm::Type::HalfTyID:
//                    break;
//
//                case llvm::Type::FloatTyID:
//                    break;
//
//                case llvm::Type::DoubleTyID:
//                    break;
//
//                case llvm::Type::X86_FP80TyID:
//                    break;
//
//                case llvm::Type::FP128TyID:
//                    break;
//
//                case llvm::Type::PPC_FP128TyID:
//                    break;
//
//                case llvm::Type::LabelTyID:
//                    break;
//
//                case llvm::Type::MetadataTyID:
//                    break;
//
//                case llvm::Type::X86_MMXTyID:
//                    break;
//
                case llvm::Type::IntegerTyID:
                    switch( to_type->getIntegerBitWidth() ) {
                    case 32:
                        gv.IntVal = llvm::APInt(
                            32,
                            *static_cast<std::int32_t const* const>( storage )
                            );
                        break;

                    case 64:
                        gv.IntVal = llvm::APInt(
                            64,
                            *static_cast<std::int64_t const* const>( storage )
                            );
                        break;

                    default:
                        debug_out << "bitwidth: " << to_type->getIntegerBitWidth() << std::endl;
                        assert( false && "[ice] bitwidth" );
                    }
                    break;
//
//                case llvm::Type::FunctionTyID:
//                    break;
//
//                case llvm::Type::StructTyID:
//                    break;
//
//                case llvm::Type::ArrayTyID:
//                    break;
//
                case llvm::Type::PointerTyID:
                    gv.PointerVal = storage;
                    break;
//
//                case llvm::Type::VectorTyID:
//                    break;
                default:
                    assert( false && "[ice] type" );
                }

                return gv;
            }

        } // namespace llvm_engine
    } // namespace compile_time
} // namespace rill
