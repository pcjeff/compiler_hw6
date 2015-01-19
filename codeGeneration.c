#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "header.h"
#include "symbolTable.h"
#include "offsetInAR.h"
#include "myRegister.h"
#include "printSourceFile.h"

FILE* g_codeGenOutputFp = NULL;
char* g_currentFunctionName = NULL;

int getLabelNumber();
int codeGenConstantLabel(C_type constantType, void* valuePtr);
void codeGenGetBoolOfFloat(int boolRegIndex, int floatRegIndex);
void codeGenPrepareRegister(ProcessorType processorType, int regIndex, int needToBeLoaded, int workRegIndexIfPseudo, char** regName);
void codeGenSaveToMemoryIfPsuedoRegister(ProcessorType processorType, int regIndex, char* regName);
void codeGenFloatCompInstruction(char *instruction, int dstRegIndex, int srcReg1Index, int srcReg2Index);
void codeGenLogicalInstruction(ProcessorType processorType, char *instruction, int dstRegIndex, int srcReg1Index, int srcReg2Index);
//reg1 is dst
void codeGen2RegInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index);
//reg1 is dst
void codeGen3RegInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index, int reg3Index);
void codeGen2Reg1ImmInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index, void* imm);
int codeGenConvertFromIntToFloat(int intRegIndex);
int codeGenConvertFromFloatToInt(int floatRegIndex);
//*************************
void codeGenVariable(AST_NODE *varaibleDeclListNode);


void codeGenProgramNode(AST_NODE *programNode);
void codeGenGlobalVariable(AST_NODE *varaibleDeclListNode);
void codeGenFunctionDeclaration(AST_NODE *functionDeclNode);
void codeGenGeneralNode(AST_NODE* node);
void codeGenStmtNode(AST_NODE* stmtNode);
void codeGenBlockNode(AST_NODE* blockNode);
void codeGenWhileStmt(AST_NODE* whileStmtNode);
void codeGenForStmt(AST_NODE* forStmtNode);
void codeGenIfStmt(AST_NODE* ifStmtNode);
void codeGenReturnStmt(AST_NODE* returnStmtNode);
void codeGenAssignOrExpr(AST_NODE* testNode);
void codeGenAssignmentStmt(AST_NODE* assignmentStmtNode);
void codeGenExprRelatedNode(AST_NODE* exprRelatedNode);
void codeGenExprNode(AST_NODE* exprNode);
void codeGenFunctionCall(AST_NODE* functionCallNode);
void codeGenVariableReference(AST_NODE* idNode);
void codeGenConstantReference(AST_NODE* constantNode);
int codeGenCalcArrayElemenetAddress(AST_NODE* idNode);

int getLabelNumber()
{
    static int labelNumber = 0;
    return labelNumber++;
}


int codeGenConstantLabel(C_type constantType, void* valuePtr)
{
    int labelNumber = getLabelNumber();
    
    fprintf(g_codeGenOutputFp, ".data\n");

    if(constantType == INTEGERC)
    {
        int* val = (int*)valuePtr;
        fprintf(g_codeGenOutputFp, "_CONSTANT_%d: .word %d\n", labelNumber, *val);
    }
    else if(constantType == FLOATC)
    {
        float* val = (float*)valuePtr;
        fprintf(g_codeGenOutputFp, "_CONSTANT_%d: .float %f\n", labelNumber, *val);
    }
    else if(constantType == STRINGC)
    {
        char* val;
	val = (char *)valuePtr;
	val[strlen(valuePtr)-1]='\0';
        fprintf(g_codeGenOutputFp, "_CONSTANT_%d: .ascii %s", labelNumber, val);
        fprintf(g_codeGenOutputFp, "\\000\"\n");
        fprintf(g_codeGenOutputFp, ".align 2\n");
	val[strlen(valuePtr)-1]='"';
	val[strlen(valuePtr)]='\0';
    }

    fprintf(g_codeGenOutputFp, ".text\n");

    return labelNumber;
}

void codeGenSetReg(ProcessorType processorType, char* instruction, int reg1Index, int value){
	char* reg1Name = NULL;
	codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);
	fprintf(g_codeGenOutputFp, "%s %s, #%d\n",instruction, reg1Name, value);
	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}

void codeGenGetBoolOfFloat(int boolRegIndex, int floatRegIndex)
{

    float zero = 0.0f;
    int constantZeroLabelNumber = codeGenConstantLabel(FLOATC, &zero);
    char* boolRegName = NULL;
    codeGenPrepareRegister(INT_REG, boolRegIndex, 0, 0, &boolRegName);

    char* tmpZeroRegName = floatWorkRegisterName[0];
    fprintf(g_codeGenOutputFp, "ldr %s, =_CONSTANT_%d\n", boolRegName, constantZeroLabelNumber);
    fprintf(g_codeGenOutputFp, "vldr.f32 %s, [%s,#0]\n", tmpZeroRegName, boolRegName);
    char* origFloatRegName = NULL;
    codeGenPrepareRegister(FLOAT_REG, floatRegIndex, 1, 1, &origFloatRegName);
    fprintf(g_codeGenOutputFp, "vcmp.f32 %s, %s\n", tmpZeroRegName, origFloatRegName);
    
    
    fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
    codeGenSetReg(INT_REG, "mov",boolRegIndex, 0);
    codeGenSetReg(INT_REG, "moveq",boolRegIndex, 1);

    codeGenSaveToMemoryIfPsuedoRegister(INT_REG, boolRegIndex, boolRegName);
}



void codeGenPrepareRegister(ProcessorType processorType, int regIndex, int needToBeLoaded, int workRegIndexIfPseudo, char** regName)
{
    int realRegisterCount = (processorType == INT_REG) ? INT_REGISTER_COUNT : FLOAT_REGISTER_COUNT;
    char** realRegisterName = (processorType == INT_REG) ? intRegisterName : floatRegisterName;
    char** workRegisterName = (processorType == INT_REG) ? intWorkRegisterName : floatWorkRegisterName;
    char* loadInstruction = (processorType == INT_REG) ? "ldr" : "vldr.f32";

    if(regIndex >= realRegisterCount)
    {
        //pseudo register
        int pseudoIndex = regIndex - realRegisterCount;
        *regName = workRegisterName[workRegIndexIfPseudo];
        if(needToBeLoaded)
        {
            fprintf(g_codeGenOutputFp, "%s %s, [fp, #%d]\n", loadInstruction, *regName, getPseudoRegisterCorrespondingOffset(pseudoIndex));
        }
    }
    else
    {
        *regName = realRegisterName[regIndex];
    }
}


void codeGenSaveToMemoryIfPsuedoRegister(ProcessorType processorType, int regIndex, char* regName)
{
    int realRegisterCount = (processorType == INT_REG) ? INT_REGISTER_COUNT : FLOAT_REGISTER_COUNT;
    char* saveInstruction = (processorType == INT_REG) ? "str" : "vstr.f32";

    if(regIndex >= realRegisterCount)
    {
        //pseudo register
        int pseudoIndex = regIndex - realRegisterCount;
        fprintf(g_codeGenOutputFp, "%s %s, [fp, #%d]\n", saveInstruction, regName, getPseudoRegisterCorrespondingOffset(pseudoIndex));
    }
}


void codeGenFloatCompInstruction(char *notRealInstruction, int dstRegIndex, int srcReg1Index, int srcReg2Index)
{
    char* srcReg1Name = NULL;
    codeGenPrepareRegister(FLOAT_REG, srcReg1Index, 1, 0, &srcReg1Name);

    char* srcReg2Name = NULL;
    codeGenPrepareRegister(FLOAT_REG, srcReg2Index, 1, 1, &srcReg2Name);

    char* dstRegName = NULL;
    codeGenPrepareRegister(INT_REG, dstRegIndex, 0, 0, &dstRegName);

    char* realInstruction = notRealInstruction;
    int resultOfCompareTrue = 1;
    if(strcmp(notRealInstruction, "c.ne.s") == 0)
    {
        realInstruction = "c.eq.s";
        resultOfCompareTrue = 0;
    }
    else if(strcmp(notRealInstruction, "c.ge.s") == 0)
    {
        realInstruction = "c.lt.s";
        resultOfCompareTrue = 0;
    }
    else if(strcmp(notRealInstruction, "c.gt.s") == 0)
    {
        realInstruction = "c.le.s";
        resultOfCompareTrue = 0;
    }
    
    int tmpLabelIndex = getLabelNumber();
    fprintf(g_codeGenOutputFp, "%s %s, %s\n", realInstruction, srcReg1Name, srcReg2Name);
    fprintf(g_codeGenOutputFp, "bc1f _compareFalse_%d\n", tmpLabelIndex);
    fprintf(g_codeGenOutputFp, "li %s, %d\n", dstRegName, resultOfCompareTrue);
    fprintf(g_codeGenOutputFp, "j _compareEnd_%d\n", tmpLabelIndex);
    fprintf(g_codeGenOutputFp, "_compareFalse_%d:\n", tmpLabelIndex);
    fprintf(g_codeGenOutputFp, "li %s, %d\n", dstRegName, !resultOfCompareTrue);
    fprintf(g_codeGenOutputFp, "_compareEnd_%d:\n", tmpLabelIndex);
    
    codeGenSaveToMemoryIfPsuedoRegister(INT_REG, dstRegIndex, dstRegName);
}




void codeGen1Reg1ImmInstruction(ProcessorType processorType, char* instruction, int reg1Index, int *value){
	char* reg1Name = NULL;
	codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);


	if(processorType == INT_REG){
		fprintf(g_codeGenOutputFp, "%s %s, #%d\n", instruction, reg1Name, *((int *)value) );
	}

	codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}


void codeGenLogicalInstruction(ProcessorType processorType, char *instruction, int dstRegIndex, int srcReg1Index, int srcReg2Index)
{
    int boolReg1Index = -1;
    int boolReg2Index = -1;

    if(processorType == FLOAT_REG)
    {
        boolReg1Index = getRegister(INT_REG);
        boolReg2Index = getRegister(INT_REG);
        codeGenGetBoolOfFloat(boolReg1Index, srcReg1Index);
        codeGenGetBoolOfFloat(boolReg2Index, srcReg2Index);
    }
    else if(processorType == INT_REG)
    {
        int zero = 0;
        boolReg1Index = srcReg1Index;
        boolReg2Index = srcReg2Index;
	codeGen1Reg1ImmInstruction(INT_REG, "cmp", srcReg1Index, &zero);
	codeGenSetReg(INT_REG, "mov",boolReg1Index, 0);
	codeGenSetReg(INT_REG, "movne",boolReg1Index, 1);
	codeGen1Reg1ImmInstruction(INT_REG, "cmp", srcReg2Index, &zero);
	codeGenSetReg(INT_REG, "mov",boolReg2Index, 0);
	codeGenSetReg(INT_REG, "movne",boolReg2Index, 1);

    }

    codeGen3RegInstruction(INT_REG, instruction, dstRegIndex, boolReg1Index, boolReg2Index);
    
    if(processorType == FLOAT_REG)
    {
        freeRegister(INT_REG, boolReg1Index);
        freeRegister(INT_REG, boolReg2Index);
    }
}


void codeGenCmp0Instruction(ProcessorType processorType, char* instruction, int reg1Index, int imm){

	
    char* reg1Name = NULL;
    codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);
    fprintf(g_codeGenOutputFp, "%s %s, #%d\n", instruction, reg1Name, imm);
    codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);

}


void codeGen2RegInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index)
{
    char* reg1Name = NULL;
    codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);
    
    char* reg2Name = NULL;
    codeGenPrepareRegister(processorType, reg2Index, 1, 1, &reg2Name);
    
    fprintf(g_codeGenOutputFp, "%s %s, %s\n", instruction, reg1Name, reg2Name);

    codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}

void codeGen3RegInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index, int reg3Index)
{
    char* reg1Name = NULL;
    codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);
    
    char* reg2Name = NULL;
    codeGenPrepareRegister(processorType, reg2Index, 1, 0, &reg2Name);
    
    char* reg3Name = NULL;
    codeGenPrepareRegister(processorType, reg3Index, 1, 1, &reg3Name);
    
    fprintf(g_codeGenOutputFp, "%s %s, %s, %s\n", instruction, reg1Name, reg2Name, reg3Name);

    codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}


void codeGen2Reg1ImmInstruction(ProcessorType processorType, char* instruction, int reg1Index, int reg2Index, void* imm)
{
    char* reg1Name = NULL;
    codeGenPrepareRegister(processorType, reg1Index, 0, 0, &reg1Name);
    
    char* reg2Name = NULL;
    codeGenPrepareRegister(processorType, reg2Index, 1, 0, &reg2Name);
    
    if(processorType == INT_REG)
    {
        int* val = (int*)imm;
        fprintf(g_codeGenOutputFp, "%s %s, %s, #%d\n", instruction, reg1Name, reg2Name, *val);
    }
    else if(processorType == FLOAT_REG)
    {
        float* val = (float*)imm;
        fprintf(g_codeGenOutputFp, "%s %s, %s, %f\n", instruction, reg1Name, reg2Name, *val);
    }

    codeGenSaveToMemoryIfPsuedoRegister(processorType, reg1Index, reg1Name);
}


int codeGenConvertFromIntToFloat(int intRegIndex)
{
    /*TODO*/
    int floatRegisterIndex;
    char* reg1Name = NULL;
    floatRegisterIndex = getRegister(FLOAT_REG);
    codeGenPrepareRegister(FLOAT_REG, floatRegisterIndex, 0, 0, &reg1Name);

    char* reg2Name = NULL;
    codeGenPrepareRegister(INT_REG, intRegIndex, 1, 0, &reg2Name);

    fprintf(g_codeGenOutputFp, "vmov.f32 %s, %s\n", reg1Name, reg2Name);
    fprintf(g_codeGenOutputFp, "vcvt.f32.s32 %s, %s\n", reg1Name, reg1Name);

    codeGenSaveToMemoryIfPsuedoRegister(FLOAT_REG, floatRegisterIndex, reg1Name);
    freeRegister(INT_REG, intRegIndex);
    return floatRegisterIndex;
}


int codeGenConvertFromFloatToInt(int floatRegIndex)
{
    /*TODO*/
    int intRegisterIndex;
    char* reg1Name = NULL;
    intRegisterIndex = getRegister(INT_REG);
    codeGenPrepareRegister(INT_REG, intRegisterIndex, 0, 0, &reg1Name);

    char* reg2Name = NULL;
    codeGenPrepareRegister(FLOAT_REG, floatRegIndex, 1, 0, &reg2Name);

    fprintf(g_codeGenOutputFp, "vcvt.s32.f32 %s, %s\n", reg2Name, reg2Name);
    fprintf(g_codeGenOutputFp, "vmov.f32 %s, %s\n", reg1Name, reg2Name);

    codeGenSaveToMemoryIfPsuedoRegister(INT_REG, intRegisterIndex, reg1Name);
    freeRegister(FLOAT_REG, floatRegIndex);
    return intRegisterIndex;
}


void codeGenerate(AST_NODE *root)
{
    char* outputfileName = "output.s";
    g_codeGenOutputFp = fopen(outputfileName, "w");
    if(!g_codeGenOutputFp)
    {
        printf("Cannot open file \"%s\"", outputfileName);
        exit(EXIT_FAILURE);
    }

    codeGenProgramNode(root);
}


void codeGenProgramNode(AST_NODE *programNode)
{
    AST_NODE *traverseDeclaration = programNode->child;
    while(traverseDeclaration)
    {
        if(traverseDeclaration->nodeType == VARIABLE_DECL_LIST_NODE)
        {
            fprintf(g_codeGenOutputFp, ".data\n");
            codeGenGlobalVariable(traverseDeclaration);
            fprintf(g_codeGenOutputFp, ".text\n");
        }
        else if(traverseDeclaration->nodeType == DECLARATION_NODE)
        {
            codeGenFunctionDeclaration(traverseDeclaration);
        }
        traverseDeclaration = traverseDeclaration->rightSibling;
    }
    return;
}
void codeGenVariable(AST_NODE *varaibleDeclListNode)
{
    AST_NODE *traverseDeclaration = varaibleDeclListNode->child;
    while(traverseDeclaration)
    {
        if(traverseDeclaration->semantic_value.declSemanticValue.kind == VARIABLE_DECL)
        {
            AST_NODE *idNode = traverseDeclaration->child->rightSibling;
            while(idNode)
            {
                SymbolTableEntry* idSymbolTableEntry = idNode->semantic_value.identifierSemanticValue.symbolTableEntry;
                TypeDescriptor* idTypeDescriptor = idSymbolTableEntry->attribute->attr.typeDescriptor;
                if(idTypeDescriptor->kind == SCALAR_TYPE_DESCRIPTOR)
                {
                    //type conversion
                    if(idNode->semantic_value.identifierSemanticValue.kind == WITH_INIT_ID)
                    {
                        AST_NODE* val = idNode->child;
                        
                        if(idNode->child->semantic_value.const1->const_type == INTEGERC)
                        {
                            char* reg1Name = NULL;
                            if(val->dataType == FLOAT_TYPE)
                            {
                                //idNode->registerIndex = 
                            }
                            else
                            {}
                        }
                        else if(idNode->child->semantic_value.const1->const_type == FLOATC)
                        {
                            char* reg1Name = NULL;
                            if(val->dataType == INT_TYPE)
                            {
                                idNode->registerIndex = getRegister(INT_REG);
                                codeGenPrepareRegister(INT_REG, idNode->registerIndex,0, 0, &reg1Name);
                                fprintf(g_codeGenOutputFp, "mov %s, #%d\n", reg1Name, 
                                     val->semantic_value.const1->const_u.intval);
                                idNode->registerIndex = codeGenConvertFromIntToFloat(idNode->registerIndex);
                                codeGenPrepareRegister(FLOAT_REG, idNode->registerIndex, 0, 0, &reg1Name);
                            }
                            else
                            {
                                char* reg2Name = NULL;
                                idNode->registerIndex = getRegister(FLOAT_REG);
                                codeGenPrepareRegister(FLOAT_REG, idNode->registerIndex, 0, 0, &reg1Name);
                                int temp_reg = getRegister(INT_REG);
                                codeGenPrepareRegister(INT_REG, temp_reg, 0, 0, &reg2Name);
                                float temp = val->semantic_value.const1->const_u.fval;
                                int constantZeroLabelNumber = codeGenConstantLabel(FLOATC, &temp);
                                fprintf(g_codeGenOutputFp, "ldr %s, =_CONSTANT_%d\n", reg2Name, constantZeroLabelNumber);
                                fprintf(g_codeGenOutputFp, "vldr.f32 %s, [%s,#0]\n", reg1Name, reg2Name);
                                freeRegister(INT_REG, temp_reg);
                            }
                            //
                            fprintf(g_codeGenOutputFp, "vstr.f32 %s, [fp, #%d]\n", reg1Name, idSymbolTableEntry->attribute->offsetInAR);
                            freeRegister(FLOAT_REG, idNode->registerIndex);
                        }
                    }
                }
                idNode = idNode->rightSibling;
            }
        }
        traverseDeclaration = traverseDeclaration->rightSibling;
    }
}

void codeGenGlobalVariable(AST_NODE* varaibleDeclListNode)
{
    AST_NODE *traverseDeclaration = varaibleDeclListNode->child;
    while(traverseDeclaration)
    {
        if(traverseDeclaration->semantic_value.declSemanticValue.kind == VARIABLE_DECL)
        {
            AST_NODE *idNode = traverseDeclaration->child->rightSibling;
            while(idNode)
            {
                SymbolTableEntry* idSymbolTableEntry = idNode->semantic_value.identifierSemanticValue.symbolTableEntry;
                TypeDescriptor* idTypeDescriptor = idSymbolTableEntry->attribute->attr.typeDescriptor;
                if(idTypeDescriptor->kind == SCALAR_TYPE_DESCRIPTOR)
                {
                    //type conversion
                    if(idNode->semantic_value.identifierSemanticValue.kind == WITH_INIT_ID)
                    {
                        AST_NODE* val = idNode->child;

                        if(idTypeDescriptor->properties.dataType == INT_TYPE)
                        {
                            if(val->dataType == FLOAT_TYPE)
                                fprintf(g_codeGenOutputFp, "_g_%s: .word %d\n", idSymbolTableEntry->name, (int)val->semantic_value.const1->const_u.fval);
                            else
                                fprintf(g_codeGenOutputFp, "_g_%s: .word %d\n", idSymbolTableEntry->name, val->semantic_value.const1->const_u.intval);
                        }
                        else if(idTypeDescriptor->properties.dataType == FLOAT_TYPE)
                        {
                            if(val->dataType == INT_TYPE)
                                fprintf(g_codeGenOutputFp, "_g_%s: .float %f\n", idSymbolTableEntry->name, (float)val->semantic_value.const1->const_u.intval);
                            else
                                fprintf(g_codeGenOutputFp, "_g_%s: .float %f\n", idSymbolTableEntry->name, val->semantic_value.const1->const_u.fval);
                        }
                    }
                    else
                    {
                        if(idTypeDescriptor->properties.dataType == INT_TYPE)
                        {
                            if(idNode)
                            fprintf(g_codeGenOutputFp, "_g_%s: .word 0\n", idSymbolTableEntry->name);
                        }
                        else if(idTypeDescriptor->properties.dataType == FLOAT_TYPE)
                        {
                            fprintf(g_codeGenOutputFp, "_g_%s: .float 0.0\n", idSymbolTableEntry->name);
                        }
                    }
                    
                }
                else if(idTypeDescriptor->kind == ARRAY_TYPE_DESCRIPTOR)
                {
                    int variableSize = getVariableSize(idTypeDescriptor);
                    fprintf(g_codeGenOutputFp, "_g_%s: .space %d\n", idSymbolTableEntry->name, variableSize);
                }
                idNode = idNode->rightSibling;
            }
        }
        traverseDeclaration = traverseDeclaration->rightSibling;
    }
    return;
}


void codeGenFunctionDeclaration(AST_NODE *functionDeclNode)
{
    AST_NODE* functionIdNode = functionDeclNode->child->rightSibling;
    int i;
    
    g_currentFunctionName = functionIdNode->semantic_value.identifierSemanticValue.identifierName;

    fprintf(g_codeGenOutputFp, ".text\n");
    if (strcmp(functionIdNode->semantic_value.identifierSemanticValue.identifierName, "main") != 0) {
        fprintf(g_codeGenOutputFp, "_start_%s:\n", functionIdNode->semantic_value.identifierSemanticValue.identifierName);
    } else {
        fprintf(g_codeGenOutputFp, "%s:\n", functionIdNode->semantic_value.identifierSemanticValue.identifierName);
    }
    
    //prologue
    fprintf(g_codeGenOutputFp, "str lr, [sp, #0]\n");
    fprintf(g_codeGenOutputFp, "str fp, [sp, #-4]\n");
    fprintf(g_codeGenOutputFp, "add fp, sp, #-4\n");
    fprintf(g_codeGenOutputFp, "add sp, sp, #-8\n");
    fprintf(g_codeGenOutputFp, "ldr lr, =_frameSize_%s\n", functionIdNode->semantic_value.identifierSemanticValue.identifierName);
    fprintf(g_codeGenOutputFp, "ldr lr, [lr, #0]\n");
    fprintf(g_codeGenOutputFp, "sub sp, sp, lr\n");
    printStoreRegister(g_codeGenOutputFp);

    resetRegisterTable(functionIdNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->offsetInAR);

    AST_NODE* blockNode = functionIdNode->rightSibling->rightSibling;
    AST_NODE *traverseListNode = blockNode->child;
    while(traverseListNode)
    {
        codeGenGeneralNode(traverseListNode);
        traverseListNode = traverseListNode->rightSibling;
    }

    //epilogue
    fprintf(g_codeGenOutputFp, "_end_%s:\n", g_currentFunctionName);
    printRestoreRegister(g_codeGenOutputFp);
    fprintf(g_codeGenOutputFp, "ldr lr, [fp, #4]\n");
    fprintf(g_codeGenOutputFp, "mov sp, fp\n");
    fprintf(g_codeGenOutputFp, "add sp, sp, #4\n");
    fprintf(g_codeGenOutputFp, "ldr fp, [fp,#0]\n");
    if (strcmp(functionIdNode->semantic_value.identifierSemanticValue.identifierName, "main") == 0)
    {
    }
    else
    {
        fprintf(g_codeGenOutputFp, "bx lr\n");
    }
    fprintf(g_codeGenOutputFp, ".data\n");
    int frameSize = abs(functionIdNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->offsetInAR) + 
        (INT_REGISTER_COUNT + INT_WORK_REGISTER_COUNT + FLOAT_REGISTER_COUNT + FLOAT_WORK_REGISTER_COUNT) * 4 +
        g_pseudoRegisterTable.isAllocatedVector->size * 4;
	while(frameSize%8 != 0){
		frameSize=frameSize+4;	
	}
    fprintf(g_codeGenOutputFp, "_frameSize_%s: .word %d\n", functionIdNode->semantic_value.identifierSemanticValue.identifierName, frameSize);
    return;
}


void codeGenBlockNode(AST_NODE* blockNode)
{
    AST_NODE *traverseListNode = blockNode->child;
    while(traverseListNode)
    {
        codeGenGeneralNode(traverseListNode);
        traverseListNode = traverseListNode->rightSibling;
    }
}


void codeGenExprNode(AST_NODE* exprNode)
{
    if(exprNode->semantic_value.exprSemanticValue.kind == BINARY_OPERATION)
    {
        AST_NODE* leftOp = exprNode->child;
        AST_NODE* rightOp = leftOp->rightSibling;
        codeGenExprRelatedNode(leftOp);
        codeGenExprRelatedNode(rightOp);
        if(leftOp->dataType == FLOAT_TYPE || rightOp->dataType == FLOAT_TYPE)
        {
            if(leftOp->dataType == INT_TYPE)
            {
                leftOp->registerIndex = codeGenConvertFromIntToFloat(leftOp->registerIndex);
            }
            if(rightOp->dataType == INT_TYPE)
            {
                rightOp->registerIndex = codeGenConvertFromIntToFloat(rightOp->registerIndex);
            }
            
            switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp)
            {
            case BINARY_OP_ADD:
                exprNode->registerIndex = leftOp->registerIndex;
                codeGen3RegInstruction(FLOAT_REG, "vadd.f32", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            case BINARY_OP_SUB:
                exprNode->registerIndex = leftOp->registerIndex;
                codeGen3RegInstruction(FLOAT_REG, "vsub.f32", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            case BINARY_OP_MUL:
                exprNode->registerIndex = leftOp->registerIndex;
                codeGen3RegInstruction(FLOAT_REG, "vmul.f32", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            case BINARY_OP_DIV:
                exprNode->registerIndex = leftOp->registerIndex;
                codeGen3RegInstruction(FLOAT_REG, "vdiv.f32", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            case BINARY_OP_EQ:
                exprNode->registerIndex = getRegister(INT_REG);
        		codeGen2RegInstruction(FLOAT_REG, "vcmp.f32", leftOp->registerIndex, rightOp->registerIndex);
        		fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
        		codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
        		codeGenSetReg(INT_REG, "moveq",exprNode->registerIndex, 1);
                freeRegister(FLOAT_REG, leftOp->registerIndex);
                break;
            case BINARY_OP_GE:
                exprNode->registerIndex = getRegister(INT_REG);
        		codeGen2RegInstruction(FLOAT_REG, "vcmp.f32", leftOp->registerIndex, rightOp->registerIndex);
        		fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
        		codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
        		codeGenSetReg(INT_REG, "movge",exprNode->registerIndex, 1);
                freeRegister(FLOAT_REG, leftOp->registerIndex);
                break;
            case BINARY_OP_LE:
                exprNode->registerIndex = getRegister(INT_REG);
        		codeGen2RegInstruction(FLOAT_REG, "vcmp.f32", leftOp->registerIndex, rightOp->registerIndex);
        		fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
        		codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
        		codeGenSetReg(INT_REG, "movle",exprNode->registerIndex, 1);
                freeRegister(FLOAT_REG, leftOp->registerIndex);
                break;
            case BINARY_OP_NE:
                exprNode->registerIndex = getRegister(INT_REG);
        		codeGen2RegInstruction(FLOAT_REG, "vcmp.f32", leftOp->registerIndex, rightOp->registerIndex);
        		fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
        		codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
        		codeGenSetReg(INT_REG, "movne",exprNode->registerIndex, 1);
                freeRegister(FLOAT_REG, leftOp->registerIndex);
                break;
            case BINARY_OP_GT:
                exprNode->registerIndex = getRegister(INT_REG);
        		codeGen2RegInstruction(FLOAT_REG, "vcmp.f32", leftOp->registerIndex, rightOp->registerIndex);
        		fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
        		codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
        		codeGenSetReg(INT_REG, "movgt",exprNode->registerIndex, 1);
                freeRegister(FLOAT_REG, leftOp->registerIndex);
                break;
            case BINARY_OP_LT:
                exprNode->registerIndex = getRegister(INT_REG);
        		codeGen2RegInstruction(FLOAT_REG, "vcmp.f32", leftOp->registerIndex, rightOp->registerIndex);
        		fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
        		codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
        		codeGenSetReg(INT_REG, "movlt",exprNode->registerIndex, 1);
                freeRegister(FLOAT_REG, leftOp->registerIndex);
                break;
            case BINARY_OP_AND:
                exprNode->registerIndex = getRegister(INT_REG);
                codeGenLogicalInstruction(FLOAT_REG, "and", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                freeRegister(FLOAT_REG, leftOp->registerIndex);
                break;
            case BINARY_OP_OR:
                exprNode->registerIndex = getRegister(INT_REG);
                codeGenLogicalInstruction(FLOAT_REG, "orr", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                freeRegister(FLOAT_REG, leftOp->registerIndex);
                break;
            default:
                printf("Unhandled case in void evaluateExprValue(AST_NODE* exprNode)\n");
                break;
            }

            freeRegister(FLOAT_REG, rightOp->registerIndex);
        }
        else if(exprNode->dataType == INT_TYPE)
        {
            exprNode->registerIndex = leftOp->registerIndex;
            switch(exprNode->semantic_value.exprSemanticValue.op.binaryOp)
            {
            case BINARY_OP_ADD:
                codeGen3RegInstruction(INT_REG, "add", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            case BINARY_OP_SUB:
                codeGen3RegInstruction(INT_REG, "sub", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            case BINARY_OP_MUL:
                codeGen3RegInstruction(INT_REG, "mul", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            case BINARY_OP_DIV:
                codeGen3RegInstruction(INT_REG, "sdiv", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            case BINARY_OP_EQ:
	            codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
                codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
                codeGenSetReg(INT_REG, "moveq",exprNode->registerIndex, 1);
                break;
            case BINARY_OP_GE:
	            codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
                codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
                codeGenSetReg(INT_REG, "movge",exprNode->registerIndex, 1);
                break;
            case BINARY_OP_LE:
	            codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
                codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
                codeGenSetReg(INT_REG, "movle",exprNode->registerIndex, 1);
                break;
            case BINARY_OP_NE:
	            codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
                codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
                codeGenSetReg(INT_REG, "movne",exprNode->registerIndex, 1);
                break;
            case BINARY_OP_GT:
	            codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
                codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
                codeGenSetReg(INT_REG, "movgt",exprNode->registerIndex, 1);
                break;
            case BINARY_OP_LT:
	            codeGen2RegInstruction(INT_REG, "cmp", leftOp->registerIndex, rightOp->registerIndex);
                codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
                codeGenSetReg(INT_REG, "movlt",exprNode->registerIndex, 1);
                break;
            case BINARY_OP_AND:
                codeGenLogicalInstruction(INT_REG, "and", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            case BINARY_OP_OR:
                codeGenLogicalInstruction(INT_REG, "orr", exprNode->registerIndex, leftOp->registerIndex, rightOp->registerIndex);
                break;
            default:
                printf("Unhandled case in void evaluateExprValue(AST_NODE* exprNode)\n");
                break;
            }

            freeRegister(INT_REG, rightOp->registerIndex);
        }
    }//endif BINARY_OPERATION
    else if(exprNode->semantic_value.exprSemanticValue.kind == UNARY_OPERATION)
    {
        int tmpZero = 0;
        AST_NODE* operand = exprNode->child;
        codeGenExprRelatedNode(operand);
        if(operand->dataType == FLOAT_TYPE)
        {
            switch(exprNode->semantic_value.exprSemanticValue.op.unaryOp)
            {
            case UNARY_OP_POSITIVE:
                exprNode->registerIndex = operand->registerIndex;
                break;
            case UNARY_OP_NEGATIVE:
                exprNode->registerIndex = operand->registerIndex;
                codeGen2RegInstruction(FLOAT_REG, "vneg.f32", exprNode->registerIndex, exprNode->registerIndex);
                break;
            case UNARY_OP_LOGICAL_NEGATION:
                exprNode->registerIndex = getRegister(INT_REG);
                codeGenGetBoolOfFloat(exprNode->registerIndex, operand->registerIndex);
                freeRegister(FLOAT_REG, operand->registerIndex);
                break;
            default:
                printf("Unhandled case in void evaluateExprValue(AST_NODE* exprNode)\n");
                break;
            }
        }
        else if(operand->dataType == INT_TYPE)
        {
            switch(exprNode->semantic_value.exprSemanticValue.op.unaryOp)
            {
            case UNARY_OP_POSITIVE:
                exprNode->registerIndex = operand->registerIndex;
                break;
            case UNARY_OP_NEGATIVE:
                exprNode->registerIndex = operand->registerIndex;
                codeGen2RegInstruction(INT_REG, "neg", exprNode->registerIndex, exprNode->registerIndex);
                break;
            case UNARY_OP_LOGICAL_NEGATION:
                exprNode->registerIndex = operand->registerIndex;
		        codeGenCmp0Instruction(INT_REG,"cmp",exprNode->registerIndex,0);
                codeGenSetReg(INT_REG, "mov",exprNode->registerIndex, 0);
                codeGenSetReg(INT_REG, "moveq",exprNode->registerIndex, 1);
                break;
            default:
                printf("Unhandled case in void evaluateExprValue(AST_NODE* exprNode)\n");
                break;
            }
        }
    }
}


void codeGenFunctionCall(AST_NODE* functionCallNode)
{
    AST_NODE* functionIdNode = functionCallNode->child;
    AST_NODE* parameterList = functionIdNode->rightSibling;
    if(strcmp(functionIdNode->semantic_value.identifierSemanticValue.identifierName, "write") == 0)
    {
        AST_NODE* firstParameter = parameterList->child;
        codeGenExprRelatedNode(firstParameter);
        char* parameterRegName = NULL;
        switch(firstParameter->dataType)
        {
        case INT_TYPE:
	    codeGenPrepareRegister(INT_REG, firstParameter->registerIndex, 1, 0, &parameterRegName);
	    fprintf(g_codeGenOutputFp, "mov r0, %s\n", parameterRegName);
	    fprintf(g_codeGenOutputFp, "bl _write_int\n");
            freeRegister(INT_REG, firstParameter->registerIndex);
            break;
        case FLOAT_TYPE:
	    codeGenPrepareRegister(FLOAT_REG, firstParameter->registerIndex, 1, 0, &parameterRegName);
	    fprintf(g_codeGenOutputFp, "vmov s0, %s\n", parameterRegName);
	    fprintf(g_codeGenOutputFp, "bl _write_float\n");
            freeRegister(FLOAT_REG, firstParameter->registerIndex);
            break;
        case CONST_STRING_TYPE:
	    codeGenPrepareRegister(INT_REG, firstParameter->registerIndex, 1, 0, &parameterRegName);
	    fprintf(g_codeGenOutputFp, "mov r0, %s\n", parameterRegName);
	    fprintf(g_codeGenOutputFp, "bl _write_str\n");
            freeRegister(INT_REG, firstParameter->registerIndex);
            break;
        default:
            printf("Unhandled case in void codeGenFunctionCall(AST_NODE* functionCallNode)\n");
            printf("firstParameter->registerIndex was not free\n");
            break;
        }
        return;
    }


    if(strcmp(functionIdNode->semantic_value.identifierSemanticValue.identifierName, "read") == 0)
    {
        fprintf(g_codeGenOutputFp, "bl _read_int\n");
    }
    else if(strcmp(functionIdNode->semantic_value.identifierSemanticValue.identifierName, "fread") == 0)
    {
        fprintf(g_codeGenOutputFp, "bl _read_float\n");
    }
    else
    {
        if (strcmp(functionIdNode->semantic_value.identifierSemanticValue.identifierName, "main") != 0) {
            fprintf(g_codeGenOutputFp, "bl _start_%s\n", functionIdNode->semantic_value.identifierSemanticValue.identifierName);
        } else {
            fprintf(g_codeGenOutputFp, "bl %s\n", functionIdNode->semantic_value.identifierSemanticValue.identifierName);
        }
    }




    if (functionIdNode->semantic_value.identifierSemanticValue.symbolTableEntry) {
        if(functionIdNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.functionSignature->returnType == INT_TYPE)
        {
            functionCallNode->registerIndex = getRegister(INT_REG);
            char* returnIntRegName = NULL;
            codeGenPrepareRegister(INT_REG, functionCallNode->registerIndex, 0, 0, &returnIntRegName);

            fprintf(g_codeGenOutputFp, "mov %s, r0\n", returnIntRegName);

            codeGenSaveToMemoryIfPsuedoRegister(INT_REG, functionCallNode->registerIndex, returnIntRegName);
        }
        else if(functionIdNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.functionSignature->returnType == FLOAT_TYPE)
        {
            functionCallNode->registerIndex = getRegister(FLOAT_REG);
            char* returnfloatRegName = NULL;
            codeGenPrepareRegister(FLOAT_REG, functionCallNode->registerIndex, 0, 0, &returnfloatRegName);

            fprintf(g_codeGenOutputFp, "vmov %s, s0\n", returnfloatRegName);

            codeGenSaveToMemoryIfPsuedoRegister(INT_REG, functionCallNode->registerIndex, returnfloatRegName);
        }
    }
}


int codeGenCalcArrayElemenetAddress(AST_NODE* idNode)
{
    AST_NODE* traverseDim = idNode->child;
    int* sizeInEachDimension = idNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->attr.typeDescriptor->properties.arrayProperties.sizeInEachDimension;
            
    codeGenExprRelatedNode(traverseDim);
    int linearIdxRegisterIndex = traverseDim->registerIndex;
    traverseDim = traverseDim->rightSibling;

    int dimIndex = 1;
    /*TODO multiple dimensions
    while(traverseDim)
    {
    }
    */
    
    int shiftLeftTwoBits = 2;
    codeGen2Reg1ImmInstruction(INT_REG, "lsl", linearIdxRegisterIndex, linearIdxRegisterIndex, &shiftLeftTwoBits);
    
    char* linearOffsetRegName = NULL;
    if(!isGlobalVariable(idNode->semantic_value.identifierSemanticValue.symbolTableEntry))
    {
        int baseOffset = idNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->offsetInAR;
        codeGen2Reg1ImmInstruction(INT_REG, "add", linearIdxRegisterIndex, linearIdxRegisterIndex, &baseOffset);
        codeGenPrepareRegister(INT_REG, linearIdxRegisterIndex, 1, 0, &linearOffsetRegName);
        fprintf(g_codeGenOutputFp, "add %s, %s, fp\n", linearOffsetRegName, linearOffsetRegName);
    }
    else
    {
        fprintf(g_codeGenOutputFp, "ldr %s,= _g_%s\n", intWorkRegisterName[0], idNode->semantic_value.identifierSemanticValue.identifierName);
        codeGenPrepareRegister(INT_REG, linearIdxRegisterIndex, 1, 1, &linearOffsetRegName);
        fprintf(g_codeGenOutputFp, "add %s, %s, %s\n", linearOffsetRegName, linearOffsetRegName, intWorkRegisterName[0]);
    }

    codeGenSaveToMemoryIfPsuedoRegister(INT_REG, linearIdxRegisterIndex, linearOffsetRegName);

    return linearIdxRegisterIndex;
}

void codeGenVariableReference(AST_NODE* idNode)
{
    SymbolAttribute *idAttribute = idNode->semantic_value.identifierSemanticValue.symbolTableEntry->attribute;
    if(idNode->semantic_value.identifierSemanticValue.kind == NORMAL_ID)
    {
        if(idNode->dataType == INT_TYPE)
        {
            idNode->registerIndex = getRegister(INT_REG);
            char* loadRegName = NULL;
            if(!isGlobalVariable(idNode->semantic_value.identifierSemanticValue.symbolTableEntry))
            {
                codeGenPrepareRegister(INT_REG, idNode->registerIndex, 0, 0, &loadRegName);
                fprintf(g_codeGenOutputFp, "ldr %s, [fp, #%d]\n", loadRegName, idAttribute->offsetInAR);
            }
            else
            {
                fprintf(g_codeGenOutputFp, "ldr %s, =_g_%s\n", intWorkRegisterName[0], idNode->semantic_value.identifierSemanticValue.identifierName);
                codeGenPrepareRegister(INT_REG, idNode->registerIndex, 0, 1, &loadRegName);
                fprintf(g_codeGenOutputFp, "ldr %s, [%s,#0]\n", loadRegName, intWorkRegisterName[0]);
            }
            codeGenSaveToMemoryIfPsuedoRegister(INT_REG, idNode->registerIndex, loadRegName);
        }
        else if(idNode->dataType == FLOAT_TYPE)
        {
            idNode->registerIndex = getRegister(FLOAT_REG);
            char* loadRegName = NULL;
            if(!isGlobalVariable(idNode->semantic_value.identifierSemanticValue.symbolTableEntry))
            {
                codeGenPrepareRegister(FLOAT_REG, idNode->registerIndex, 0, 0, &loadRegName);
                fprintf(g_codeGenOutputFp, "vldr.f32 %s, [fp, #%d]\n", loadRegName, idAttribute->offsetInAR);
            }
            else
            {
                fprintf(g_codeGenOutputFp, "ldr %s, =_g_%s\n", intWorkRegisterName[0], idNode->semantic_value.identifierSemanticValue.identifierName);
                codeGenPrepareRegister(FLOAT_REG, idNode->registerIndex, 0, 0, &loadRegName);
                fprintf(g_codeGenOutputFp, "vldr.f32 %s, [%s, #0]\n", loadRegName, intWorkRegisterName[0]);
            }
            codeGenSaveToMemoryIfPsuedoRegister(FLOAT_REG, idNode->registerIndex, loadRegName);
        }
    }
    else if(idNode->semantic_value.identifierSemanticValue.kind == ARRAY_ID)
    {
        if(idNode->dataType == INT_TYPE || idNode->dataType == FLOAT_TYPE)
        {
            int elementAddressRegIndex = codeGenCalcArrayElemenetAddress(idNode);
            char* elementAddressRegName = NULL;
            codeGenPrepareRegister(INT_REG, elementAddressRegIndex, 1, 0, &elementAddressRegName);
            
            if(idNode->dataType == INT_TYPE)
            {
                idNode->registerIndex = elementAddressRegIndex;
                fprintf(g_codeGenOutputFp, "ldr %s, [%s, #0]\n", elementAddressRegName, elementAddressRegName);
                codeGenSaveToMemoryIfPsuedoRegister(INT_REG, idNode->registerIndex, elementAddressRegName);
            }
            else if(idNode->dataType == FLOAT_TYPE)
            {
                idNode->registerIndex = getRegister(FLOAT_REG);
                char* dstRegName = NULL;
                codeGenPrepareRegister(FLOAT_REG, idNode->registerIndex, 0, 0, &dstRegName);
                
                char* elementAddressRegName = NULL;
                codeGenPrepareRegister(INT_REG, elementAddressRegIndex, 1, 0, &elementAddressRegName);
            
                fprintf(g_codeGenOutputFp, "vldr.f32 %s, [%s, #0]\n", dstRegName, elementAddressRegName);
                codeGenSaveToMemoryIfPsuedoRegister(FLOAT_REG, idNode->registerIndex, dstRegName);
            
                freeRegister(INT_REG, elementAddressRegIndex);
            }
        }
    }
}

void codeGenConstantReference(AST_NODE* constantNode)
{
    C_type cType = constantNode->semantic_value.const1->const_type;
    if(cType == INTEGERC)
    {
        int tmpInt = constantNode->semantic_value.const1->const_u.intval;
        int constantLabelNumber = codeGenConstantLabel(INTEGERC, &tmpInt);
        constantNode->registerIndex = getRegister(INT_REG);
        char* regName = NULL;
        codeGenPrepareRegister(INT_REG, constantNode->registerIndex, 0, 0, &regName);
        fprintf(g_codeGenOutputFp, "ldr %s, =_CONSTANT_%d\n", regName, constantLabelNumber);
        fprintf(g_codeGenOutputFp, "ldr %s, [%s, #0]\n", regName, regName);
        codeGenSaveToMemoryIfPsuedoRegister(INT_REG, constantNode->registerIndex, regName);
    }
    else if(cType == FLOATC)
    {
        float tmpFloat = constantNode->semantic_value.const1->const_u.fval;
        int constantLabelNumber = codeGenConstantLabel(FLOATC, &tmpFloat);
        constantNode->registerIndex = getRegister(FLOAT_REG);
        char* regName = NULL;
        codeGenPrepareRegister(FLOAT_REG, constantNode->registerIndex, 0, 0, &regName);
        fprintf(g_codeGenOutputFp, "ldr %s, =_CONSTANT_%d\n", intWorkRegisterName[0], constantLabelNumber);
        fprintf(g_codeGenOutputFp, "vldr.f32 %s, [%s, #0]\n", regName, intWorkRegisterName[0]);
        codeGenSaveToMemoryIfPsuedoRegister(FLOAT_REG, constantNode->registerIndex, regName);
    }
    else if(cType == STRINGC)
    {
        char* tmpCharPtr = constantNode->semantic_value.const1->const_u.sc;
        int constantLabelNumber = codeGenConstantLabel(STRINGC, tmpCharPtr);
        constantNode->registerIndex = getRegister(INT_REG);
        char* regName = NULL;
        codeGenPrepareRegister(INT_REG, constantNode->registerIndex, 0, 0, &regName);
        fprintf(g_codeGenOutputFp, "ldr %s, =_CONSTANT_%d\n", regName, constantLabelNumber);
        codeGenSaveToMemoryIfPsuedoRegister(INT_REG, constantNode->registerIndex, regName);
    }
}


void codeGenExprRelatedNode(AST_NODE* exprRelatedNode)
{
    switch(exprRelatedNode->nodeType)
    {
    case EXPR_NODE:
        codeGenExprNode(exprRelatedNode);
        break;
    case STMT_NODE:
        codeGenFunctionCall(exprRelatedNode);
        break;
    case IDENTIFIER_NODE:
        codeGenVariableReference(exprRelatedNode);
        break;
    case CONST_VALUE_NODE:
        codeGenConstantReference(exprRelatedNode);
        break;
    default:
        printf("Unhandle case in void processExprRelatedNode(AST_NODE* exprRelatedNode)\n");
        exprRelatedNode->dataType = ERROR_TYPE;
        break;
    }
}

void codeGenAssignmentStmt(AST_NODE* assignmentStmtNode)
{
    AST_NODE* leftOp = assignmentStmtNode->child;
    AST_NODE* rightOp = leftOp->rightSibling;
    codeGenExprRelatedNode(rightOp);

    /* TODO type conversion */

    if(leftOp->semantic_value.identifierSemanticValue.kind == NORMAL_ID)
    {
        if(leftOp->dataType == INT_TYPE)
        {
            char* rightOpRegName = NULL;
            //type conversion
            if(rightOp->dataType == FLOAT_TYPE)
            {
                rightOp->registerIndex = codeGenConvertFromFloatToInt(rightOp->registerIndex);
            }

            codeGenPrepareRegister(INT_REG, rightOp->registerIndex, 1, 0, &rightOpRegName);
            
            if(!isGlobalVariable(leftOp->semantic_value.identifierSemanticValue.symbolTableEntry))
            {
                fprintf(g_codeGenOutputFp, "str %s, [fp, #%d]\n", rightOpRegName, leftOp->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->offsetInAR);
            }
            else
            {		
            	int tmp_reg_index = getRegister(INT_REG);
            	char *tmp_reg_name = intRegisterName[tmp_reg_index] ;
                fprintf(g_codeGenOutputFp,"ldr %s, =_g_%s\n",tmp_reg_name,leftOp->semantic_value.identifierSemanticValue.identifierName);
            	fprintf(g_codeGenOutputFp,"str %s, [%s, #0]\n",rightOpRegName, tmp_reg_name);
            	freeRegister(INT_REG, tmp_reg_index);	
            }
            leftOp->registerIndex = rightOp->registerIndex;
        }
        else if(leftOp->dataType == FLOAT_TYPE)
        {
            char* rightOpRegName = NULL;
            //type conversion
            if(rightOp->dataType == INT_TYPE)
            {
                rightOp->registerIndex = codeGenConvertFromIntToFloat(rightOp->registerIndex);
            }

            codeGenPrepareRegister(FLOAT_REG, rightOp->registerIndex, 1, 0, &rightOpRegName);
            
            if(!isGlobalVariable(leftOp->semantic_value.identifierSemanticValue.symbolTableEntry))
            {
                fprintf(g_codeGenOutputFp, "vstr.f32 %s, [fp, #%d]\n", rightOpRegName, leftOp->semantic_value.identifierSemanticValue.symbolTableEntry->attribute->offsetInAR);
            }
            else
            {
		        int tmp_reg_index = getRegister(INT_REG);
		        char *tmp_reg_name = intRegisterName[tmp_reg_index] ;
                fprintf(g_codeGenOutputFp,"ldr %s, =_g_%s\n",tmp_reg_name,leftOp->semantic_value.identifierSemanticValue.identifierName);
                fprintf(g_codeGenOutputFp, "vstr.f32 %s, [%s, #0]\n", rightOpRegName,tmp_reg_name);
	            freeRegister(INT_REG, tmp_reg_index);	
            }
            leftOp->registerIndex = rightOp->registerIndex;
        }
    }
    else if(leftOp->semantic_value.identifierSemanticValue.kind == ARRAY_ID)
    {
        int elementAddressRegIndex = codeGenCalcArrayElemenetAddress(leftOp);
        //can't be test before finishing codeGenCalcArrayElemenetAddress
        char* elementAddressRegName = NULL;
        codeGenPrepareRegister(INT_REG, elementAddressRegIndex, 1, 0, &elementAddressRegName);
        if(leftOp->dataType == INT_TYPE)
        {
            char* rightOpRegName = NULL;
            if(rightOp->dataType == FLOAT_TYPE)
            {
                rightOp->registerIndex = codeGenConvertFromFloatToInt(rightOp->registerIndex);
            }
            codeGenPrepareRegister(INT_REG, rightOp->registerIndex, 1, 1, &rightOpRegName);
            fprintf(g_codeGenOutputFp, "str %s, [%s, #0]\n", rightOpRegName, elementAddressRegName);
            
            leftOp->registerIndex = rightOp->registerIndex;
        }
        else if(leftOp->dataType == FLOAT_TYPE)
        {
            char* rightOpRegName = NULL;
            if(rightOp->dataType == INT_TYPE)
            {
                rightOp->registerIndex = codeGenConvertFromIntToFloat(rightOp->registerIndex);
            }
            codeGenPrepareRegister(FLOAT_REG, rightOp->registerIndex, 1, 0, &rightOpRegName);
            
            fprintf(g_codeGenOutputFp, "vstr %s, [%s, #0]\n", rightOpRegName, elementAddressRegName);

            leftOp->registerIndex = rightOp->registerIndex;
        }

        freeRegister(INT_REG, elementAddressRegIndex);
    }
}


void codeGenAssignOrExpr(AST_NODE* testNode)
{
    if(testNode->nodeType == STMT_NODE)
    {
        if(testNode->semantic_value.stmtSemanticValue.kind == ASSIGN_STMT)
        {
            codeGenAssignmentStmt(testNode);
        }
        else if(testNode->semantic_value.stmtSemanticValue.kind == FUNCTION_CALL_STMT)
        {
            codeGenFunctionCall(testNode);
        }
    }
    else
    {
        codeGenExprRelatedNode(testNode);
    }
}


void codeGenWhileStmt(AST_NODE* whileStmtNode)
{
    AST_NODE* boolExpression = whileStmtNode->child;

    int constantZeroLabelNumber = -1;
    if(boolExpression->dataType == FLOAT_TYPE)
    {
        float zero = 0.0f;
        constantZeroLabelNumber = codeGenConstantLabel(FLOATC, &zero);
    }

    int labelNumber = getLabelNumber();
    fprintf(g_codeGenOutputFp, "_whileTestLabel_%d:\n", labelNumber);
    
    codeGenAssignOrExpr(boolExpression);

    if(boolExpression->dataType == INT_TYPE)
    {
        char* boolRegName = NULL;
        codeGenPrepareRegister(INT_REG, boolExpression->registerIndex, 1, 0, &boolRegName);
        fprintf(g_codeGenOutputFp, "cmp %s, #0\n", boolRegName);
        fprintf(g_codeGenOutputFp, "beq _whileExitLabel_%d\n",labelNumber);
        freeRegister(INT_REG, boolExpression->registerIndex);
    }
    else if(boolExpression->dataType == FLOAT_TYPE)
    {
        fprintf(g_codeGenOutputFp, "vldr.f32 %s, _CONSTANT_%d\n", floatWorkRegisterName[0], constantZeroLabelNumber);
        char* boolRegName = NULL;
        codeGenPrepareRegister(FLOAT_REG, boolExpression->registerIndex, 1, 1, &boolRegName);
        fprintf(g_codeGenOutputFp, "vcmp.f32 %s, %s\n", boolRegName, floatWorkRegisterName[0]);
	fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
        fprintf(g_codeGenOutputFp, "beq _whileExitLabel_%d\n", labelNumber);
        freeRegister(FLOAT_REG, boolExpression->registerIndex);
    }
    
    AST_NODE* bodyNode = boolExpression->rightSibling;
    codeGenStmtNode(bodyNode);

    fprintf(g_codeGenOutputFp, "b _whileTestLabel_%d\n", labelNumber);
    fprintf(g_codeGenOutputFp, "_whileExitLabel_%d:\n", labelNumber);
}


void codeGenForStmt(AST_NODE* forStmtNode)
{
    /*TODO*/
}


void codeGenIfStmt(AST_NODE* ifStmtNode)
{
    AST_NODE* boolExpression = ifStmtNode->child;

    int constantZeroLabelNumber = -1;
    if(boolExpression->dataType == FLOAT_TYPE)
    {
        float zero = 0.0f;
        constantZeroLabelNumber = codeGenConstantLabel(FLOATC, &zero);
    }

    int labelNumber = getLabelNumber();

    codeGenAssignOrExpr(boolExpression);

    if(boolExpression->dataType == INT_TYPE)
    {
        char* boolRegName = NULL;
        codeGenPrepareRegister(INT_REG, boolExpression->registerIndex, 1, 0, &boolRegName);
        fprintf(g_codeGenOutputFp, "cmp %s, #0\n", boolRegName);
        fprintf(g_codeGenOutputFp, "beq _elseLabel_%d\n", labelNumber);
        freeRegister(INT_REG, boolExpression->registerIndex);
    }
    else if(boolExpression->dataType == FLOAT_TYPE)
    {
	fprintf(g_codeGenOutputFp, "vldr.f32 %s, _CONSTANT_%d\n", floatWorkRegisterName[0], constantZeroLabelNumber);
	char* boolRegName = NULL;
	codeGenPrepareRegister(FLOAT_REG, boolExpression->registerIndex, 1, 1, &boolRegName);
	fprintf(g_codeGenOutputFp, "vcmp.f32 %s, %s\n", boolRegName, floatWorkRegisterName[0]);
	fprintf(g_codeGenOutputFp, "vmrs  APSR_nzcv, FPSCR\n");
	fprintf(g_codeGenOutputFp, "beq _whileExitLabel_%d\n", labelNumber);
	freeRegister(FLOAT_REG, boolExpression->registerIndex);
        codeGenPrepareRegister(FLOAT_REG, boolExpression->registerIndex, 1, 1, &boolRegName);
    }

    AST_NODE* ifBodyNode = boolExpression->rightSibling;
    codeGenStmtNode(ifBodyNode);
    
    fprintf(g_codeGenOutputFp, "b _ifExitLabel_%d\n", labelNumber);
    fprintf(g_codeGenOutputFp, "_elseLabel_%d:\n", labelNumber);
    AST_NODE* elsePartNode = ifBodyNode->rightSibling;
    codeGenStmtNode(elsePartNode);
    fprintf(g_codeGenOutputFp, "_ifExitLabel_%d:\n", labelNumber);
}


void codeGenReturnStmt(AST_NODE* returnStmtNode)
{
    AST_NODE* returnVal = returnStmtNode->child;
    if(returnVal->nodeType != NUL_NODE)
    {
        codeGenExprRelatedNode(returnVal);
        /* TODO type conversion */

        char* returnValRegName = NULL;
        if (returnStmtNode->dataType == INT_TYPE)
        {
            if(returnVal->dataType == FLOAT_TYPE)
            {
                returnVal->registerIndex = codeGenConvertFromFloatToInt(returnVal->registerIndex);
            }
            codeGenPrepareRegister(INT_REG, returnVal->registerIndex, 1, 0, &returnValRegName);
            fprintf(g_codeGenOutputFp, "mov r0, %s\n", returnValRegName);
            freeRegister(INT_REG, returnVal->registerIndex);
        }
        else if(returnStmtNode->dataType == FLOAT_TYPE)
        {
            if(returnVal->dataType == INT_TYPE)
            {
                returnVal->registerIndex = codeGenConvertFromIntToFloat(returnVal->registerIndex);
            }
            codeGenPrepareRegister(FLOAT_REG, returnVal->registerIndex, 1, 0, &returnValRegName);
            fprintf(g_codeGenOutputFp, "vmov s0, %s\n", returnValRegName);
            freeRegister(FLOAT_REG, returnVal->registerIndex);
        }
    }
    fprintf(g_codeGenOutputFp, "b _end_%s\n", g_currentFunctionName); 
}


void codeGenStmtNode(AST_NODE* stmtNode)
{
    printSourceFile(g_codeGenOutputFp, stmtNode->linenumber);

    if(stmtNode->nodeType == NUL_NODE)
    {
        return;
    }
    else if(stmtNode->nodeType == BLOCK_NODE)
    {
        codeGenBlockNode(stmtNode);
    }
    else
    {
        switch(stmtNode->semantic_value.stmtSemanticValue.kind)
        {
        case WHILE_STMT:
            codeGenWhileStmt(stmtNode);
            break;
        case FOR_STMT:
            codeGenForStmt(stmtNode);
            break;
        case ASSIGN_STMT:
            codeGenAssignmentStmt(stmtNode);
            if(stmtNode->child->dataType == INT_TYPE)
            {
                freeRegister(INT_REG, stmtNode->child->registerIndex);
            }
            else if(stmtNode->child->dataType == FLOAT_TYPE)
            {
                freeRegister(FLOAT_REG, stmtNode->child->registerIndex);
            }
            break;
        case IF_STMT:
            codeGenIfStmt(stmtNode);
            break;
        case FUNCTION_CALL_STMT:
            codeGenFunctionCall(stmtNode);
            if(stmtNode->registerIndex != -1)
            {
                if(stmtNode->dataType == INT_TYPE)
                {
                    freeRegister(INT_REG, stmtNode->registerIndex);
                }
                else if(stmtNode->dataType == FLOAT_TYPE)
                {
                    freeRegister(FLOAT_REG, stmtNode->registerIndex);
                }
            }
            break;
        case RETURN_STMT:
            codeGenReturnStmt(stmtNode);
            break;
        default:
            printf("Unhandle case in void processStmtNode(AST_NODE* stmtNode)\n");
            break;
        }
    }
}


void codeGenGeneralNode(AST_NODE* node)
{
    AST_NODE *traverseChildren = node->child;
    switch(node->nodeType)
    {
    case VARIABLE_DECL_LIST_NODE:
        codeGenVariable(node);
        break;
    case STMT_LIST_NODE:
        while(traverseChildren)
        {
            codeGenStmtNode(traverseChildren);
            traverseChildren = traverseChildren->rightSibling;
        }
        break;
    case NONEMPTY_ASSIGN_EXPR_LIST_NODE:
        while(traverseChildren)
        {
            codeGenAssignOrExpr(traverseChildren);
            if(traverseChildren->rightSibling)
            {
                if(traverseChildren->dataType == INT_TYPE)
                {
                    freeRegister(INT_REG, traverseChildren->registerIndex);
                }
                else if(traverseChildren->dataType == FLOAT_TYPE)
                {
                    freeRegister(FLOAT_REG, traverseChildren->registerIndex);
                }
            }
            traverseChildren = traverseChildren->rightSibling;
        }
        node->registerIndex = traverseChildren->registerIndex;
        break;
    case NONEMPTY_RELOP_EXPR_LIST_NODE:
        while(traverseChildren)
        {
            codeGenExprRelatedNode(traverseChildren);
            if(traverseChildren->rightSibling)
            {
                if(traverseChildren->dataType == INT_TYPE)
                {
                    freeRegister(INT_REG, traverseChildren->registerIndex);
                }
                else if(traverseChildren->dataType == FLOAT_TYPE)
                {
                    freeRegister(FLOAT_REG, traverseChildren->registerIndex);
                }
            }
            traverseChildren = traverseChildren->rightSibling;
        }
        node->registerIndex = traverseChildren->registerIndex;
        break;
    case NUL_NODE:
        break;
    default:
        printf("Unhandle case in void processGeneralNode(AST_NODE *node)\n");
        node->dataType = ERROR_TYPE;
        break;
    }
}
