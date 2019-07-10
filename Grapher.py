#!/usr/bin/env python
# coding: utf-8

# ------------------------------------- #
# LOAD ALL DATA AND ASSIGN TO VARIABLES #
# ------------------------------------- #

import matplotlib.pyplot as plt
import matplotlib.ticker
import subprocess
import os
import math
import shutil
import numpy as np

# GET CURRENT WORKING DIRECTORY AND ENUMERATE DATA FILE NUMBERS
cwd = os.getcwd()
data = (1,2,3,4)

for i in range (1,5):
    # LOAD DATA OBTAINED FROM FITer.exe PROGRAM AND COPIED INTO CORRECT FILES
    # READ X AXES LOCATIONS
    temp1 = np.loadtxt(cwd + "\\data\\dato" + str(data[i-1]) + "\\abs.txt")
    # READ APPARENT RESISTIVITIES AS OBSERVED IN THE FIELD
    temp2 = np.loadtxt(cwd + "\\data\\dato" + str(data[i-1]) + "\\fielddata.txt")
    # READ THICKNESSES FOR PROPOSED MODEL
    temp3 = np.loadtxt(cwd + "\\data\\dato" + str(data[i-1]) + "\\thicks.txt")
    # READ RESISTIVITIES FOR PROPOSED MODEL
    temp4 = np.loadtxt(cwd + "\\data\\dato" + str(data[i-1]) + "\\rhos.txt")
    
    # GENERATE A PROPER INPUT FOR oneill.exe AND EXECUTE
    # USE shutil.copyfile TO TEMPORARILY SET THE CORRECT Abs.dat FILE CONTENTS
    shutil.copyfile(cwd + "\\data\\dato" + str(data[i-1]) + "\\abs.txt", cwd+"\\Abs.dat") 
    # START GENERATING oneill.exe SHELL INPUT STRING
    oneillInput = ""
    oneillInput += str(len(temp4)) + "\n"
    for n in range(len(temp4)):
        oneillInput += str(temp4[n]) + "\n"
    for n in range(len(temp3)):
        oneillInput += str(temp3[n]) + "\n"
    
    # RUN SUBPROCESS oneill.exe TO GENERATE THEORETICAL RESISITIVY FROM PROPOSED CRV
    process=subprocess.Popen(['bin\\oneill.exe'],
                         stdin=subprocess.PIPE,
                         stdout=subprocess.PIPE,
                         stderr=subprocess.PIPE)
    # PIPE SHELL INPUT STRING
    inputdata=oneillInput.encode()
    stdoutdata,stderrdata=process.communicate(input=inputdata)
    
    # READ THEORETICAL RESISTIVITY
    temp5 = np.loadtxt(cwd + "\\RA.dat")
    # READ ZOHDY INTERPOLATED FIELD DATA 
    temp6 = np.loadtxt(cwd + "\\data\\dato" + str(data[i-1]) + "\\interpzohdy.txt")

    
    # PUSH TEMP VARIABLES TO PROPER VARIABLES
    exec("abs" + str(i) + " = temp1")
    exec("roc" + str(i) + " = temp2")
    exec("thicks" + str(i) + " = temp3")
    exec("rhos" + str(i) + " = temp4")
    exec("rac" + str(i) + " = temp5[:,1]")
    exec("zoh" + str(i) + " = temp6")

# --------------------- #
# DATA PLOTTING FUCTION #
# --------------------- #

def plotData(axes,rhos,thicks,calculated,observed,interped,savenumber):   
    
    # PREPARE VARIABLES FOR PROPOSED CRV STEP PLOT
    # GENERATE A VECTOR OF DEPTHS FOR CEILING OF EACH LAYER
    depths = np.array([0])
    totalDepth = 0
    for idx in range(len(thicks)):
        totalDepth += thicks[idx]
        depths = np.append(depths,np.array([totalDepth]))
    rhos = np.append(rhos1, rhos1[len(rhos1)-1])
    depths = np.append(depths,1000)
    
    # FIND RMS BETWEEN INTERP. OBSERVED AND CALCULATED APPARENT RESISTIVITIES
    rms = 0
    for idx in range(len(interped)):
        rms += ((interped[idx] - calculated[idx])/interped[idx]) ** 2
    rms = math.sqrt( rms / len(interped))
    
    # GET TITLES
    titletext = input("Titulo de grafico para el SEV " + str(savenumber) + ": ")

    
    # SET FIGURE, SIZE AND RESOLUTION 1000X1000
    plt.figure(0)
    plt.figure(num=None, figsize=(10, 10), dpi=300, facecolor='w', edgecolor='k')

    # SET FITURE STYLE, AXES, TITLE
    plt.style.use('classic')
    ax = plt.axes(xscale='log', yscale='log')
    ax.grid();
    ax.grid(b=True, which='major', color='black', linestyle='-')
    ax.grid(b=True, which='minor', color='grey', linestyle='--')
    plt.title(titletext)
    plt.xlabel("Profundidad (m)")
    plt.ylabel("Resistividad (Ωm)")

    # PLOT RAW FIELD CURVE DATA
    plt.plot(axes,observed, linewidth = 2.0, marker = "o", markersize = 5, color = "red", alpha=0.7, label = "CRA observado")
    ax.legend(loc="upper left", borderaxespad=0.)

    # SAVE PLOT
    plt.savefig("fieldcurve" + str(savenumber) + ".png")
    
    
    # SET FIGURE, SIZE AND RESOLUTION 1000X1000
    plt.figure(1)
    plt.figure(num=None, figsize=(10, 10), dpi=300, facecolor='w', edgecolor='k')

    # SET FITURE STYLE, AXES, TITLE
    plt.style.use('classic')
    ax = plt.axes(xscale='log', yscale='log')
    ax.grid();
    ax.grid(b=True, which='major', color='black', linestyle='-')
    ax.grid(b=True, which='minor', color='grey', linestyle='--')
    plt.title(titletext)
    plt.xlabel("Profundidad (m)")
    plt.ylabel("Resistividad (Ωm)")

    # PLOT MODEL DATA
    plt.step(depths, rhos, linewidth = 2.0, alpha=0.7, color = "green", where="post", label = "CRV de modelo propuesto")
    plt.plot(axes,calculated, linewidth = 2.0,  marker = "o", markersize = 5, color = "blue", alpha=0.7, label = "CRA de modelo propuesto")
    plt.plot(axes,interped, linewidth = 2.0,  marker = "o", markersize = 5, color = "red", alpha=0.7, label = "CRA observado (interp. Zohdy)")
    ax.legend(loc="upper left", borderaxespad=0.)
    text = "RMS = " + str(rms * 100)[:4] + "%"
    ax.text(0.8, 0.95, text, bbox=dict(boxstyle = "square",facecolor = "white"), transform=ax.transAxes)
    text = "Techo Puelches: " + str(depths[2])[:2] + "m\nBase Puelches: " + str(depths[3])[:2] + "m\nEspesor Puelches: " + str(depths[3] - depths[2])[:2] + "m"
    ax.text(0.05, 0.05, text, bbox=dict(boxstyle = "square",facecolor = "white"), transform=ax.transAxes)

    # SAVE PLOT
    plt.savefig("model" + str(savenumber) + ".png")

# ------------- #
# ACTUAL OUTPUT #
# ------------- #

plotData(abs1, rhos1, thicks1, rac1, roc1, zoh1, 1)
plotData(abs2, rhos2, thicks2, rac2, roc2, zoh2, 2)
plotData(abs3, rhos3, thicks3, rac3, roc3, zoh3, 3)
plotData(abs4, rhos4, thicks4, rac4, roc4, zoh4, 4)