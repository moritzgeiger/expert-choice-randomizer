import pandas as pd
import numpy as np
from PIL import Image, ImageFont, ImageDraw
import os
import re
import shutil

# CONFIG
class BgConfig:
    width = 1600
    height = 1600
    grey = (123,123,123)
    size = (width, height)

class BgImages:
    bottle = "image/bg/Flasche_Blanko.png"
    capsule = "image/bg/Kapsel.png"
    tag = "image/bg/Preisschild.png"
    canvas = "image/bg/Blanko_Rückeneti.png"
    brand = "image/bg/Brand.png"
    
IMG_SUBFOLDER = 'image'
CSV_COLS = [
    'price1', 'price2', 'price3', 
    'grape1', 'grape2', 'grape3', 
    'seal1', 'seal2', 
    'label1', 'label2', 'label3'
]


def create_ad(
        label: str, 
#         name: str, 
        text: str, 
        variety: str, 
        price: str, 
        seal: str, 
        img_name='test.png'
    ) -> str:
    """Creates a wine ad given a set of input image path variables
    Args: 
        label: string pointing to the variable `label` image file to be pasted in the image.  
        name: string pointing to the variable `name` image file to be pasted in the image.  
        text: string pointing to the variable `text` image file to be pasted in the image.  
        variety: string pointing to the variable `variety` image file to be pasted in the image.  
        price: string pointing to the variable `price` image file to be pasted in the image.  
        seal: string pointing to the variable `seal` image file to be pasted in the image.  
        img_name: final output name of the image
    Returns:
        image_name
        """
    # config

#     font = ImageFont.truetype("fonts/font.otf", 150)

    # load images
    with Image.new('RGBA', (BgConfig.size), BgConfig.grey) as background:
        bottle = Image.open(BgImages.bottle, formats=['PNG'])
        capsule = Image.open(BgImages.capsule, formats=['PNG'])
        tag = Image.open(BgImages.tag, formats=['PNG'])
        canvas = Image.open(BgImages.canvas, formats=['PNG'])
        brand = Image.open(BgImages.brand, formats=['PNG']).convert("RGBA")

        _label = Image.open(os.path.join(IMG_SUBFOLDER, label), formats=['PNG']).convert("RGBA")
        _text = Image.open(os.path.join(IMG_SUBFOLDER, text), formats=['PNG']).convert("RGBA")
        _variety = Image.open(os.path.join(IMG_SUBFOLDER, variety), formats=['PNG']).convert("RGBA")
        _price = Image.open(os.path.join(IMG_SUBFOLDER, price), formats=['PNG']).convert("RGBA")
        _seal = Image.open(os.path.join(IMG_SUBFOLDER, seal), formats=['PNG'])

        # background
        background.paste(bottle, (18, BgConfig.height-1552-18), bottle)
        background.paste(tag, (300,30), tag)
        background.paste(capsule, (167,18), capsule)
        background.paste(canvas, (BgConfig.width-1079-35, BgConfig.height-1075-30), canvas)
        background.paste(brand, (665,600), brand)
        
        # variable content
        background.paste(_seal, (140,700), _seal)
        background.paste(_label, (30,1030), _label)
        background.paste(_text, (560,930), _text)
        background.paste(_variety, (820,790), _variety)
        background.paste(_price, (800, 160), _price)

        background.save(img_name)

        return img_name


def parse_csv(csv_path, dest_folder):
    """parses csv file with columns like: 
    question,choice,levels,price1,price2,price3,grape1,grape2,grape3,seal1,seal2,label1,label2,label3,NoChoice

    Creates a custom image for each row of the file according to the csv contents. 

    Args:
        csv_path (_type_): the path to the csv file
        dest_folder (_type_): the subfolder path where to store the images. 
    """
    df = pd.read_csv(csv_path, sep=',')
    df_clean = df[df['NoChoice'] == 0]
    choices = {}
    for i, row in df_clean.iterrows():
        name = f"{int(row['question'])}_{int(row['choice'])}"
        _choice = [key for key, val in row[CSV_COLS].to_dict().items() if val > 0]
        _choice.append(f"infos{int(row['choice'])}")
        choices[name] = _choice
        
    
    for name, choice in choices.items():
        _ = create_ad(
            label=f'{choice[3]}.png',
#             name=f'{choice[1]}.png',
            price=f'{choice[0]}.png',
            variety=f'{choice[1]}.png',
            seal=f'{choice[2]}.png',
            text=f'{choice[4]}.png',
            img_name=f'{dest_folder}/{name}.png'
        )
        
        print(name, choice)
        print(f'saved to: {_}')
    
    print('DONE.')

if __name__ == '__main__':
    _ = parse_csv(
        csv_path='piwi_question_table.csv', 
        dest_folder='compiled'
        )