from dataManagementBackbone.scripts.landing.tempToPers import tempToPers

from dataManagementBackbone.scripts.formatted.persistentToFormatted import persToForm

from dataManagementBackbone.scripts.trusted.formattedToTrusted import formattedToTrusted
from dataManagementBackbone.scripts.trusted.commonAreas import commonAreas
from dataManagementBackbone.scripts.trusted.lastOutCatImputation import lastOutCatImputation
from dataManagementBackbone.scripts.trusted.LSOANameImputation import lsoaNameImputation
from dataManagementBackbone.scripts.trusted.removeCols import removeCols
from dataManagementBackbone.scripts.trusted.removeDuplicates import removeDuplicates
from dataManagementBackbone.scripts.trusted.removeSuffixLSOAName import removeSuffix
from dataManagementBackbone.scripts.trusted.checkNAs import checkNAs

from dataManagementBackbone.scripts.exploitation.join import join

from dataAnalysisBackbone.scripts.analyticalSandbox.trainTestSets import trainTestSets
from dataAnalysisBackbone.scripts.analyticalSandbox.trainModel import trainModel
from dataAnalysisBackbone.scripts.analyticalSandbox.testModel import testModel

if __name__ == "__main__":

    sources = ['crimes', 'prices', 'economicStatus']

    tempToPers(sources)
    print("Data moved from temporal to persitent\n\n")

    persToForm(sources)
    print("Data moved from persitent to formatted\n\n")
    
    formattedToTrusted([s + 'Tables' for s in sources])
    print("Data moved from formatted to trusted\n\n")

    removeDuplicates()
    print("Removed duplicate cases\n\n")
    
    removeCols() 
    print("Removed unnecessary columns\n\n")

    removeSuffix() 
    print("Removed suffix from LSOA name (crimes)\n\n")

    commonAreas()
    print("Keep common districts\n\n")

    lsoaNameImputation()
    print("Impute any NAs in LSOA Name (crimes)\n\n")
    
    lastOutCatImputation() 
    print("Impute LastOutcomeCategory (crimes)\n\n")
    
    print("Checking for additional NAs\n\n")
    checkNAs()
    
    join()
    print("Joined prices and crimes tables\n\n")

    trainTestSets()
    print("Training and testing sets created\n\n")

    trainModel()
    print("Model trained\n\n")

    testModel()




