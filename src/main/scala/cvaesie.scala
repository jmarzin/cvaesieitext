import java.io.{File, FileOutputStream}

import com.itextpdf.text.{Document, PageSize}
import com.itextpdf.text.pdf.{PdfName, PdfNumber, PdfReader, PdfSmartCopy}
import com.itextpdf.text.pdf.parser.PdfTextExtractor

import scala.collection.mutable
import scala.sys.process._

/**
  * Created by jmarzin-cp on 22/04/2017.
  */
object cvaesie extends App with utilitairesSie{

  def getSiret(chaine: String): Option[String] = {
    var siretNumber: Option[String] = None
    val motifSiret = """(?s).*SIRET : (\d{9} *\d{5}).*""".r
    chaine match {
      case motifSiret(numero) => siretNumber = Some(numero.replaceAll(" ",""))
      case _ =>
    }
    siretNumber
  }

  def addPage(dico : scala.collection.mutable.Map[String, (Boolean,List[(PdfReader, Int)])],
              cle : String,
              fichier: PdfReader,
              ipage: Int): Unit ={
    if (dico.get(cle).isDefined) {
      dico(cle) = (dico(cle)._1,dico(cle)._2 :+ (fichier, ipage))
    } else {
      dico += (cle -> (false,List((fichier, ipage))))
    }
  }

  def traiteFic(dico: scala.collection.mutable.Map[String, (Boolean,List[(PdfReader, Int)])],
               fichier: PdfReader,
               nbPages: Int): Unit = {
    for (ipage <- 1 to fichier.getNumberOfPages) {
      val siretNumber = getSiret(PdfTextExtractor.getTextFromPage(fichier, ipage))
      if (siretNumber.isDefined) for(page <- ipage until ipage + nbPages) addPage(dico, siretNumber.get, fichier, page)
    }
  }

  def complet(relance: (String, (Boolean,List[(PdfReader, Int)]))): Boolean = {
    var relanceComplete = true
    print("siret " + relance._1)
    if(dico2807.get(relance._1).isDefined) {
      dico2807(relance._1) = (true, dico2807(relance._1)._2)
      print(" comparaison trouvée")

      for(comparaison <- dico2807(relance._1)._2) {
        val chaine = PdfTextExtractor.getTextFromPage(comparaison._1, comparaison._2)
        if(!chaine.matches("(?s).*MONTANT TOTAL PENALITES SUR SOLDE 5% : *0,00.*")) {
          if(dicoMajos5.get(relance._1).isDefined) {
            dicoMajos5(relance._1) = (true,dicoMajos5(relance._1)._2)
            print(", majoration 5 % trouvée")
          } else {
            relanceComplete = false
            print(", majoration 5 % non trouvée")
          }
        }
        if(!chaine.matches("(?s).*MONTANT MAJORATION 0\\.2% : *0,00.*")){
          if(dicoMajos02.get(relance._1).isDefined) {
            dicoMajos02(relance._1) = (true,dicoMajos02(relance._1)._2)
            print(", majoration 0,2 % trouvée")
          } else {
            relanceComplete = false
            print(", majoration 0,2 % non trouvée")
          }
        }
        println()
      }
    } else {
      relanceComplete = false
      println(" comparaisons non trouvées")
    }
    relanceComplete
  }

  val dateHeure = getLaunchTime
  val retour = getDirectoryListing("Répertoire des relances CVAE à traiter")
  if (retour._2.isEmpty) System.exit(0)
  val repertoire = retour._1
  val listeFichiers = retour._2.get

  val clicEsi = showOptions(message = "Choisissez la transformation à appliquer",
    title = "Transformation",
    entries = List("Aucune","ClicEsiPlus"),
    initial = 1).getOrElse("")
  if(clicEsi.isEmpty)System.exit(0)

  val listeFichiersLus = new mutable.Stack[(PdfReader, String)]()
  val dico2807 = scala.collection.mutable.Map[String, (Boolean,List[(PdfReader,Int)])]()
  val dicoRelances = scala.collection.mutable.Map[String, (Boolean,List[(PdfReader,Int)])]()
  val dicoMajos5 = scala.collection.mutable.Map[String, (Boolean,List[(PdfReader,Int)])]()
  val dicoMajos02 = scala.collection.mutable.Map[String, (Boolean,List[(PdfReader,Int)])]()

  for(file <- listeFichiers) {
    if (file.getName.startsWith("courriers_")) {
      println("Fichier " + file.getName + " trouvé et non traité")
    } else {
      val fic = new PdfReader(file.getCanonicalPath)
      listeFichiersLus.push((fic, file.getCanonicalPath))
      val chaine = PdfTextExtractor.getTextFromPage(fic, 1)

      if (chaine.contains("RESULTAT DE COMPARAISON CVAE") || chaine.contains("ETAT DE RESULTATS COMPARAISON CVAE")) {
        println("Fichier des résultats de comparaison CVAE trouvé")
        traiteFic(dico2807, fic, 1)

      } else if (chaine.contains("Cotisation sur la valeur ajoutée des entreprises. Régularisation au titre de l'année")) {
        println("Fichier des relances CVAE trouvé")
        traiteFic(dicoRelances, fic, 2)

      } else if (chaine.contains("Cotisation sur la valeur ajoutée des entreprises. Motivation de la majoration de 5%")) {
        println("Fichier des majorations de CVAE de 5 % trouvé")
        traiteFic(dicoMajos5, fic, 2)

      } else if (chaine.contains("Cotisation sur la valeur ajoutée des entreprises. Motivation de la majoration de 0,2%")) {
        println("Fichier des majorations de CVAE de 0.2 % trouvé")
        traiteFic(dicoMajos02, fic, 2)

      }
    }
  }

  val docCourriers = new Document()
  var copy: PdfSmartCopy = _
  val nomFichierCourriers = repertoire.getCanonicalPath + File.separatorChar + "courriers"  + "_" + dateHeure + ".pdf"

  for(relance <- dicoRelances) {

    if(complet(relance)){
      dicoRelances(relance._1) = (true,relance._2._2)
      if(copy == null) {
        copy = new PdfSmartCopy(docCourriers, new FileOutputStream(nomFichierCourriers))
        docCourriers.open()
      }
      for(page <- relance._2._2) {
        copy.addPage(copy.getImportedPage(page._1, page._2))
      }
      if(dico2807.get(relance._1).isDefined) for(page <- dico2807(relance._1)._2) {
        val pageP = page._1.getPageN(page._2)
        val rotate = pageP.getAsNumber(PdfName.ROTATE)
        if (rotate == null) pageP.put(PdfName.ROTATE, new PdfNumber(270))
        else pageP.put(PdfName.ROTATE, new PdfNumber((rotate.intValue + 270) % 360))
        copy.addPage(copy.getImportedPage(page._1, page._2))
        copy.addPage(PageSize.A4, 0)
      }
      if(dicoMajos5.get(relance._1).isDefined) for(page <- dicoMajos5(relance._1)._2) copy.addPage(copy.getImportedPage(page._1, page._2)) // pagesCourriers.add(page.clone(docsCourriers))
      if(dicoMajos02.get(relance._1).isDefined) for(page <- dicoMajos02(relance._1)._2) copy.addPage(copy.getImportedPage(page._1, page._2)) // pagesCourriers.add(page.clone(docsCourriers))
    }
  }
  if(copy != null) docCourriers.close()

  val repTraites = new File(repertoire.getCanonicalPath + File.separatorChar + "dejaTraites")
  if (!repTraites.exists() || repTraites.isFile) {
    repTraites.mkdir()
  }
  while(listeFichiersLus.nonEmpty){
    val fic = listeFichiersLus.pop
    fic._1.close()
    val nomFic = fic._2.split(File.separatorChar).last
    if(!(nomFic.startsWith("courriers_"))) {
      new File(fic._2).renameTo(new File(repertoire + File.separator + "dejaTraites" + File.separator + nomFic))
    }
  }

  dicoRelances.filter(!_._2._1).foreach(item => println("Relance Siret " + item._1 + " non traitée"))
  dico2807.filter(!_._2._1).foreach(item => println("2807 Siret " + item._1 + " non traitée"))
  dicoMajos5.filter(!_._2._1).foreach(item => println("Majo à 5 % Siret " + item._1 + " non traitée"))
  dicoMajos02.filter(!_._2._1).foreach(item => println("Majo à 0.2 % Siret " + item._1 + " non traitée"))

  //lancement de la fabrication des fichiers ClicEsi
  if(clicEsi != "Aucune") {
    var commande = "\"C:\\Program Files\\LibreOffice 4\\program\\sdraw\" " +
      "\"" + nomFichierCourriers + "\" " +
      "\"macro:///Standard.ClicEsi.ClicEsiPlus()\""
    var result = commande.!
  }
  println("Fin")
}
