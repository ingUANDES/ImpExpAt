def generateCoords(points, w, h):          #GENERA LA DISTRIBUCION DE LOS PUNTOS, Y LAS COORDENADAS CON QUE SE MOSTRARAN
    results = []
    for i in range(points):
        while True:
            xs = random.randint(10, w-10)
            ys = random.randint(10, h-10)
            coord = (xs, ys)
            if coord not in results:
                results.append(coord)
                break
    return results
def generateIncentive(incentive): 
    i = Image.new("RGB", (500, 500), "white")
    d = ImageDraw.Draw(i)
    return i
def generateImage(points, d=3):      #GENERA LA IMAGEN CON LOS PUNTOS, Y SU DISTRIBUCION 
    i = Image.new("RGB", (500, 500), "coral") #IMAGEN
    draw = ImageDraw.Draw(i)  #VARIABLE PARA EDITAR LA IMAGEN CON LOS PUNTOS
    w, h = i.size
    centers = generateCoords(points,w,h) #LISTA CON LAS COORDENADAS DE LOS PTOS
    for c in centers:
        draw.ellipse((c[0]-3, c[1]-3,c[0]+3, c[1]+3), fill="#800080") #RECORRE LA LISTA, Y DIBUJA LOS CIRCULOS
    return i
def generateSplash():  #PARA LAS INTRUCCIONES
    i = Image.new("RGB", (500, 500), "white")
    d = ImageDraw.Draw(i)
    font = ImageFont.truetype("arial.ttf", 12)
    d.multiline_text((10, 10), "EXPERIMENTO PUNTOS\n 1.A CONTINUACION, SE MOSTRARAN 100 IMAGENES QUE CONTENDRAN\n ENTRE 38 Y 42 PUNTOS\n 2. ANTES DE CADA IMAGEN DE PUNTOS,\n SE MOSTRARA EL INCENTIVO QUE USTED TIENE PARA\n VER QUE TANTO ESFUERZO LE SUGIERE CONTAR LOS PUNTOS\n 3. USTED TIENE 5 OPCIONES PARA DECIDIR\n LA RESPUESTA CORRECTA.",font=font, fill=(0, 0, 0)) #LE PONE UNA IMAGEN CON EL TEXTO SOBRE LA IMAGEN
    return i
def generateEnding():
    i = Image.new("RGB", (500, 500), "white")
    d = ImageDraw.Draw(i)
    font = ImageFont.truetype("arial.ttf", 15)
    font2 = ImageFont.truetype("arial.ttf", 20)
    d.multiline_text((10, 10), "Resultados", font=font, fill=(0, 0, 0))
    d.multiline_text((50, 100), "Preguntas correctas: "+str(RAnswerC)+"/"+str(TotPreguntas), font=font2, fill=(0, 0, 0))
    d.multiline_text((50, 300), "Tiempo total: " + str(datetime.timedelta(seconds=int(endTime)-int(startTime))), font=font2, fill=(0, 0, 0))
    return i

def nextIm(btn):  #INICIA CUANDO APRETAMOS COMENZAR, Y LUEGO SIGUE CON LAS OPCIONES DE LA CANTRIDAD DE PTOS
    global gamestate 
    global NPregunta
    global startTime
    global ans
    global RAnswerC
    global endTime
    global TotPreguntas
    global history
    correcta = False
    if gamestate!="init":     #SI ES QUE AL JUEGO ESTA EN PROCESO, SUMA EL INSENTIVO Y AL CONTADOR DE CORRECTAS O INCORRECTAS 
        if btn == ans:
            correcta = True
            RAnswerC += 1
        KPI = [char for char in incentives[NPregunta - 1][0]]
        
        KPI.extend([char for char in incentives[NPregunta - 1][1]])
        t = [NPregunta, ans, btn]
        t.extend(KPI)
        history.append(t)
        ##print(history[NPregunta-1])
    
    NPregunta += 1
    if NPregunta == TotPreguntas+1:   #TERMINO DEL JUEGO
        btn1.configure(state=DISABLED)
        btn2.configure(state=DISABLED)
        btn3.configure(state=DISABLED)
        btn4.configure(state=DISABLED)
        btn5.configure(state=DISABLED)
        endTime = timer()
        timestr = time.strftime("%Y%m%d-%H%M%S") 

        header = ['No Pregunta', 'Resp_Correcta', 'Resp_jugador', 'KPI0', 'KPI1', 'KPI2', 'KPI3', 'N0', 'N1', 'N2', 'N3']
        
        with open('Resultados/'+timestr+".csv", 'w',newline='', encoding='UTF8') as f:
            writer = csv.writer(f)
            
            # write the header
            writer.writerow(header)
            ##print(history)
            for data in history:
            # write the data
                writer.writerow(data)

        drawEnding() #IMPRIME PANTALLA DEL FINAL 

    else:
        if gamestate=="init":      #PRIMER DESAFIO, INICIA EL TIEMPO
            gamestate="started"
            startTime=timer()

            btn1.configure(state=DISABLED, text="    38    ")  #DESACTIVA LOS BOTONES MIENTRAS MUESTRA LOS INCENTIVOS
            btn2.configure(state=DISABLED, text="    39    ")
            btn3.configure(state=DISABLED, text="    40    ")
            btn4.configure(state=DISABLED, text="    41    ")
            btn5.configure(state=DISABLED, text="    42    ")
        ans = random.randint(38, 42)  #TOMA UN VALOR ENTRE 38, 42 PARA VER RESPUESTA CORRECTA 
        lb2.configure(text=str("Pregunta "+str(NPregunta)+" de "+str(TotPreguntas)))
        updateIncentives(NPregunta)
        thread = threading.Thread(target=drawPoints) #DIBUJA LA PANTALLA CON LOS INCENTIVOS
        thread.start() #ACTIVA LA LINEA ANTERIOR
        window.after(TIEMPO_ENTRE_PREGUNTAS, drawChallenge, ans)  #SETEA EL TIMER, PARA QUE NO SE QUEDE PEGADO LA IMAGEN 

def drawPoints(): #PREGUNTA CON INCENTIVO 
    newI = ImageTk.PhotoImage(generateIncentive(incentives[NPregunta - 1]))   
    lb.configure(image=newI)  #CONFIGURAR IMANEGEN PRINCIPAL
    lb.image = newI  # 
    btn1.configure(state=DISABLED, text="    38    ")
    btn2.configure(state=DISABLED, text="    39    ")
    btn3.configure(state=DISABLED, text="    40    ")
    btn4.configure(state=DISABLED, text="    41    ")
    btn5.configure(state=DISABLED, text="    42    ")

def updateIncentives(NPregunta, disabled = False):
    global incentives
    global window
    global light1
    global light2
    light1.delete('all')
    light1 = Canvas(window, width=200,height=700,bg="white")
    light1.grid(row=0,column=0,rowspan=8)
    light1.create_rectangle(5, 5, 195, 695)
    ###print(incentives[NPregunta - 1])
    for i in range(4):
        if disabled:
            fill = "white"
        elif incentives[NPregunta - 1][0][i] == '1':
            fill = "blue"
        else:
            fill = "white"
        light1.create_oval(leftpad, top+i*size+(i*pad), leftpad+size, top+(i+1)*size+i*pad, fill=fill)
        light1.create_text(int(leftpad/2), top+(i+1)*size+i*pad+10, text="KPI"+str(i))

    light2.delete('all')
    light2 = Canvas(window, width=200,height=700,bg="white")
    light2.grid(row=0,column=9,rowspan=8)
    light2.create_rectangle(5, 5, 195, 695)

    for i in range(4):
        if disabled:
            fill = "white"
        elif incentives[NPregunta - 1][1][i] == '1':
            fill = "green"
        else:
            fill = "white"
        light2.create_oval(leftpad, top+i*size+(i*pad), leftpad+size, top+(i+1)*size+i*pad, fill=fill)
        light2.create_text(int(leftpad/2), top+(i+1)*size+i*pad+10, text="NB"+str(i))
        

def drawChallenge(ans): #GENERA IMAGEN CON LOS PUNTOS
    updateIncentives(1, disabled=True)
    newI = ImageTk.PhotoImage(generateImage(ans))
    lb.configure(image=newI)
    lb.image = newI
    btn1.configure(state=NORMAL)
    btn2.configure(state=NORMAL)
    btn3.configure(state=NORMAL)
    btn4.configure(state=NORMAL)
    btn5.configure(state=NORMAL)
def drawEnding():  #GENERA IMAGEN DEL FINAL
    newI = ImageTk.PhotoImage(generateEnding())
    lb.configure(image=newI)
    lb.image = newI
    btn1.configure(state=DISABLED)
    btn2.configure(state=DISABLED)
    btn3.configure(state=DISABLED)
    btn4.configure(state=DISABLED)
    btn5.configure(state=DISABLED)
